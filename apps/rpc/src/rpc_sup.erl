%% RPC应用监督者
-module(rpc_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {rpc_registry, {rpc_registry, start_link, []}, permanent, 2000, worker, [rpc_registry]},
        {rpc_monitor, {rpc_monitor, start_link, []}, permanent, 2000, worker, [rpc_monitor]},
        {rpc_server, {rpc_server, start_link, []}, permanent, 5000, worker, [rpc_server]}
    ]}}. 