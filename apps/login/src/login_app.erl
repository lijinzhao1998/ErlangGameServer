-module(login_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start the top-level supervisor
    login_sup:start_link().

stop(_State) ->
    ok.