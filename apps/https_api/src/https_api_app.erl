-module(https_api_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % 初始化HTTPS环境
    https_init:init(),
    https_api_sup:start_link().

stop(_State) ->
    ok. 