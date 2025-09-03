%% Dynamic supervisor for player processes (modern DynamicSupervisor API)
-module(game_player_sup).

-export([start_link/0, start_player/1, stop/0]).

start_link() ->
    dynamic_supervisor:start_link({local, ?MODULE}, {one_for_one, 20, 60}).

stop() ->
    dynamic_supervisor:stop(?MODULE).

start_player(UserId) ->
    ChildSpec = #{id => {game_player, UserId},
                  start => {game_player, start_link, [UserId]},
                  restart => transient,
                  shutdown => 5000,
                  type => worker,
                  modules => [game_player]},
    dynamic_supervisor:start_child(?MODULE, ChildSpec).