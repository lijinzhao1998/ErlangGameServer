%% Public API module to start children under the DynamicSupervisor started by dynamic_supervisor_wrapper.
-module(cross_conn_sup_api).
-export([start_child/1]).

start_child(Socket) ->
    ChildSpec = #{id => cross_conn_worker,
                  start => {cross_conn_worker, start_link, [Socket]},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [cross_conn_worker]},
    dynamic_supervisor:start_child(dynamic_conn_sup, ChildSpec).