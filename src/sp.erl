-module(sp).

-behaviour(gen_server).

%% API
-export([start_link/2,
         start_link/3,
         list/1]).

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

list(ServerRef) ->
    gen_server:call(ServerRef, list).

%% gen_server

init([SerialPort, Pid]) ->
    Port = ale_util:open_port(["sp", SerialPort]),
    {ok, #state{port = Port, listener = Pid}}.

handle_call(list, _From, #state{port = Port} = State) ->
    Reply = call_port(Port, list),
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

call_port(Port, Command) ->
    Port ! {self(), {command, term_to_binary(Command)}},
    receive
        {Port, {data, <<?REPLY, Response/binary>>}} ->
            binary_to_term(Response)
    end.
