%% Top-level supervisor for cross service (starts DynamicSupervisor directly)
-module(cross_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Start a DynamicSupervisor directly as a supervised child (no wrapper).
    %% The DynamicSupervisor will be registered locally as 'dynamic_conn_sup'.
    DynChild = {dynamic_conn_sup,
                {dynamic_supervisor, start_link, [{local, dynamic_conn_sup}, {one_for_one, 20, 60}]},
                permanent, 2000, supervisor, [dynamic_supervisor]},

    Listener = {cross_listener, {cross_listener, start_link, [6000]}, permanent, 5000, worker, [cross_listener]},

    {ok, {{one_for_one, 10, 60}, [DynChild, Listener]}}.