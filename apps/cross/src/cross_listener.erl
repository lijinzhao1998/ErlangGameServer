%% Listener spawns supervised workers through cross_conn_sup:start_child(Socket)
-module(cross_listener).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, false}, {reuseaddr, true}]),
    io:format("cross_listener listening on ~p~n", [Port]),
    spawn_link(fun() -> accept_loop(LSock) end),
    {ok, #{lsock => LSock, port => Port}}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_R, _S) -> ok.
code_change(_O,_S,_E) -> {ok,_S}.

accept_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            %% Hand off socket to supervised DynamicSupervisor
            ok = maybe_start_child(Socket),
            accept_loop(LSock);
        {error, Reason} ->
            io:format("cross accept error ~p~n", [Reason]),
            timer:sleep(1000),
            accept_loop(LSock)
    end.

maybe_start_child(Socket) ->
    case cross_conn_sup:start_child(Socket) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, Pid}} ->
            io:format("connection worker already started: ~p~n", [Pid]),
            ok;
        {error, Reason} ->
            io:format("failed to start conn child: ~p~n", [Reason]),
            catch gen_tcp:close(Socket),
            {error, Reason}
    end.