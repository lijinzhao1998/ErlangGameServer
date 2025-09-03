%% RPC应用主模块
-module(rpc_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting RPC application...~n"),
    rpc_sup:start_link().

stop(_State) ->
    io:format("Stopping RPC application...~n"),
    ok. 