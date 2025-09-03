%% A small wrapper to run DynamicSupervisor under a supervisor child spec.
%% This allows the top-level cross_sup to supervise a DynamicSupervisor.
-module(dynamic_supervisor_wrapper).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Start a DynamicSupervisor and store its pid in state
    {ok, DynPid} = dynamic_supervisor:start_link({local, dynamic_conn_sup}, {one_for_one, 20, 60}),
    {ok, #{dyn => DynPid}}.

handle_call(_R,_F,State) -> {reply, ok, State}.
handle_cast(_M,State) -> {noreply, State}.
handle_info(_I,State) -> {noreply, State}.
terminate(_R,_S) -> ok.
code_change(_O,State,_E) -> {ok, State}.