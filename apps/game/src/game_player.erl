%% Example player gen_server holding player state.
-module(game_player).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(UserId) ->
    gen_server:start_link(?MODULE, UserId, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init(UserId) ->
    io:format("Starting player ~p~n", [UserId]),
    {ok, #{user => UserId, ts => erlang:system_time(seconds)}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("player terminated~n"),
    ok.

code_change(_Old, State, _Extra) -> {ok, State}.