-module(sp).

-behaviour(gen_server).

%% API
-export([start_link/2,
         start_link/3,
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
-define(NOTIFICATION, 1).

-record(state,
        {
         port       :: port(),
         listener   :: pid()
        }).

%% API

start_link(SerialPort, Pid) when is_pid(Pid) ->
    gen_server:start_link(?MODULE, [SerialPort, Pid], []).

start_link(ServerName, SerialPort, Pid) when is_pid(Pid) ->
    gen_server:start_link(ServerName, ?MODULE, [SerialPort, Pid], []).

stop(ServerRef) ->
    gen_server:call(ServerRef, stop).

list(ServerRef) ->
    gen_server:call(ServerRef, list).

write(ServerRef, Data) when is_binary(Data) ->
    gen_server:call(ServerRef, {write, Data}).

%% gen_server

init([SerialPort, Pid]) ->
    Port = ale_util:open_port(["sp", SerialPort]),
    {ok, #state{port = Port, listener = Pid}}.

handle_call(stop, _From, #state{port = Port} = State) ->
    port_close(Port),
    {stop, normal, ok, State#state{port = undefined}};
handle_call(Command, _From, #state{port = Port} = State) ->
    Reply = port_call(Port, Command),
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({Port, {data, <<?NOTIFICATION, Notif/binary>>}},
            #state{port = Port, listener = Pid} = State) ->
    Pid ! binary_to_term(Notif),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

port_call(Port, Command) ->
    Port ! {self(), {command, term_to_binary(Command)}},
    receive
        {Port, {data, <<?REPLY, Response/binary>>}} ->
            binary_to_term(Response)
    end.
