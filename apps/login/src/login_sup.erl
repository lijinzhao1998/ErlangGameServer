%% Top-level supervisor for login app
-module(login_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% restart intensity: max 5 restarts in 10 seconds
    {ok, {{one_for_one, 5, 10},
        [
            %% Listener: permanent (should always run)
            {login_listener,
             {login_listener, start_link, [5000]},
             permanent, 5000, worker, [login_listener]},
            %% Session manager: permanent
            {login_session_manager,
             {login_session_manager, start_link, []},
             permanent, 2000, worker, [login_session_manager]}
        ]}}.