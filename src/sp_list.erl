-module(sp_list).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         list/1]).

%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {port :: port()}).

%% API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(ServerName) ->
    gen_server:start_link(ServerName, ?MODULE, [], []).

list(ServerRef) ->
    gen_server:call(ServerRef, list).

%% gen_server

init([]) ->
    Port = ale_util:open_port(["sp", "list"]),
    {ok, #state{port = Port}}.

handle_call(list, _From, #state{port = Port} = State) ->
    Reply = call_port(Port, list),
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

call_port(Port, Command) ->
    Port ! {self(), {command, term_to_binary(Command)}},
    receive
        {Port, {data, Response}} -> binary_to_term(Response)
    end.
