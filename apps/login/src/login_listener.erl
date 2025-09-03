%% Listener implemented as a gen_server so it can be supervised.
%% Accepts connections and spawns login_handler processes.
-module(login_listener).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {port, lsock}).

%% API
start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

%% gen_server callbacks
init(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, false}, {reuseaddr, true}]),
    io:format("login_listener listening on ~p~n", [Port]),
    %% spawn acceptor in separate process
    spawn_link(fun() -> accept_loop(LSock) end),
    {ok, #state{port=Port, lsock=LSock}}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% accept loop (runs outside gen_server to keep socket accept non-blocking to the server)
accept_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            %% For login we spawn a short-lived handler (not under DynamicSupervisor here for simplicity)
            spawn(fun() -> login_handler:serve(Socket) end),
            accept_loop(LSock);
        {error, Reason} ->
            io:format("login accept error ~p~n", [Reason]),
            timer:sleep(1000),
            accept_loop(LSock)
    end.