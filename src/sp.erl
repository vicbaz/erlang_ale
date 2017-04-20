-module(sp).

-behaviour(gen_server).

%% API
-export([start_link/1,
         start_link/2,
         stop/1,
         list/1,
         write/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").

-define(REPLY, 0).
-define(NOTIF, 1).

-record(state,
        {
          port     :: port(),
          listener :: {pid(), reference()}
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) when is_map(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start_link(ServerName, Args) when is_map(Args) ->
    gen_server:start_link(ServerName, ?MODULE, Args, []).

stop(ServerRef) ->
    gen_server:stop(ServerRef, normal, 5000).

list(ServerRef) ->
    gen_server:call(ServerRef, list).

write(ServerRef, Data) when is_binary(Data) ->
    gen_server:call(ServerRef, {write, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(#{portname := "list"}) ->
    Port = ale_util:open_port(["sp", "list"]),
    {ok, #state{port = Port}};
init(#{portname := PortName,
       baudrate := BaudRate,
       flowcontrol := FlowControl,
       listener := Pid}) when is_pid(Pid) ->
    Port = ale_util:open_port(["sp", maybe_resolve_symlink(PortName),
                               integer_to_list(BaudRate),
                               flowcontrol_to_list(FlowControl)]),
    MRef = monitor(process, Pid),
    {ok, #state{port = Port, listener = {Pid, MRef}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Command, _From, #state{port = Port} = State) ->
    Reply = port_call(Port, Command),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {data, <<?NOTIF, Data/binary>>}},
            #state{port = Port, listener = {Pid, _}} = State) ->
    Pid ! binary_to_term(Data),
    {noreply, State};
handle_info({_Port, {exit_status, _Status} = Reason}, State) ->
    {stop, Reason, State};
handle_info({'DOWN', MRef, process, _Pid, Reason},
            #state{listener = {_, MRef}} = State) ->
    {stop, maybe_convert_reason(Reason), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_resolve_symlink(Name) ->
    case is_symlink(Name) of
        true ->
            handle_read_link(file:read_link(Name), Name);
        false ->
            Name
    end.

is_symlink(Name) ->
    {ok, Info} = file:read_link_info(Name),
    Info#file_info.type =:= symlink.

handle_read_link({ok, TargetName}, SourceName) ->
    case filename:pathtype(TargetName) of
        relative ->
            filename:join(filename:dirname(SourceName), TargetName);
        absolute ->
            TargetName
    end.

flowcontrol_to_list(none) -> "0";
flowcontrol_to_list(xonxoff) -> "1";
flowcontrol_to_list(rtscts) -> "2";
flowcontrol_to_list(dtrdsr) -> "3".

port_call(Port, Command) ->
    Port ! {self(), {command, term_to_binary(Command)}},
    receive
        {Port, {data, <<?REPLY, Data/binary>>}} ->
            binary_to_term(Data)
    after
        5000 ->
            exit(timeout)
    end.

maybe_convert_reason(normal) ->
    normal;
maybe_convert_reason(shutdown) ->
    shutdown;
maybe_convert_reason({shutdown, _Term} = Reason) ->
    Reason;
maybe_convert_reason(Reason) ->
    {listener_down, Reason}.
