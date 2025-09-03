%% Example connection worker for cross service (gen_server).
%% Started dynamically by the DynamicSupervisor.
-module(cross_conn_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    %% ensure passive receive; hand off loop to a linked process
    ok = inet:setopts(Socket, [{active, false}]),
    catch gen_tcp:send(Socket, "Welcome to cross server\r\n"),
    State = #{socket => Socket},
    spawn_link(fun() -> loop(State) end),
    {ok, State}.

loop(State = #{socket := Socket}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            io:format("cross conn got: ~p~n", [Bin]),
            catch gen_tcp:send(Socket, "CROSS-OK\r\n"),
            loop(State);
        {error, closed} ->
            io:format("client closed connection~n"),
            ok;
        {error, Reason} ->
            io:format("recv error ~p~n", [Reason]),
            ok
    end.

handle_call(_R, _F, State) -> {reply, ok, State}.
handle_cast(_M, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State = #{socket := Socket}) ->
    catch gen_tcp:close(Socket),
    ok.
code_change(_Old, State, _Extra) -> {ok, State}.