%% Top-level supervisor for game service
-module(game_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% start player dynamic supervisor and the router
    {ok, {{one_for_one, 5, 10},
        [
            {game_player_sup, {game_player_sup, start_link, []}, permanent, 2000, supervisor, [game_player_sup]},
            {game_router, {game_router, start_link, []}, permanent, 2000, worker, [game_router]}
        ]}}.