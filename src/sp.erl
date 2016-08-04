-module(sp).

-behaviour(gen_server).

%% API
-export([start_link/1,
         start_link/2,
         stop/1,
         list/1,
         write/2]).

%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(REPLY, 0).
-define(NOTIF, 1).

-record(state,
        {
          port          :: port(),
          listener      :: {pid(), reference()}
        }).

%% API

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

%% gen_server

init(#{portname := "list"}) ->
    Port = ale_util:open_port(["sp", "list"]),
    {ok, #state{port = Port}};
init(#{portname := PortName,
       baudrate := BaudRate,
       flowcontrol := FlowControl,
       listener := Pid}) when is_pid(Pid) ->
    Port = ale_util:open_port(["sp", PortName,
                               integer_to_list(BaudRate),
                               flowcontrol_to_list(FlowControl)]),
    MRef = monitor(process, Pid),
    {ok, #state{port = Port, listener = {Pid, MRef}}}.

handle_call(Command, _From, #state{port = Port} = State) ->
    Reply = port_call(Port, Command),
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({Port, {data, <<?NOTIF, Data/binary>>}},
            #state{port = Port, listener = {Pid, _}} = State) ->
    Pid ! binary_to_term(Data),
    {noreply, State};
handle_info({_Port, {exit_status, _Status} = Reason}, State) ->
    {stop, Reason, State};
handle_info({'DOWN', MRef, process, _Pid, Reason},
            #state{listener = {_, MRef}} = State) ->
    {stop, actual_reason(Reason), State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

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
            exit(port_call_timeout)
    end.

actual_reason(normal) ->
    normal;
actual_reason(shutdown) ->
    shutdown;
actual_reason({shutdown, _Term} = Reason) ->
    Reason;
actual_reason(Reason) ->
    {listener_down, Reason}.
