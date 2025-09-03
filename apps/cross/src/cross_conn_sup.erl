%% Public API module to start children under the DynamicSupervisor registered as 'dynamic_conn_sup'.
-module(cross_conn_sup).

-export([start_child/1]).

%% Start a connection worker under the DynamicSupervisor 'dynamic_conn_sup'.
%% Use a unique id per child to avoid id collisions.
start_child(Socket) ->
    ChildSpec = #{id => {cross_conn_worker, make_ref()},
                  start => {cross_conn_worker, start_link, [Socket]},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [cross_conn_worker]},
    dynamic_supervisor:start_child(dynamic_conn_sup, ChildSpec).