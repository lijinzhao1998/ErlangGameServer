%% Router for game app: simple API to create player processes via player supervisor.
-module(game_router).
-behaviour(gen_server).

-export([start_link/0, create_player/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_player(UserId) ->
    gen_server:call(?MODULE, {create_player, UserId}).

init([]) ->
    {ok, #{}}.

handle_call({create_player, UserId}, _From, State) ->
    case game_player_sup:start_player(UserId) of
        {ok, Pid} -> {reply, {ok, Pid}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call(_Other, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_M, State) -> {noreply, State}.
handle_info(_I, State) -> {noreply, State}.
terminate(_R, _S) -> ok.
code_change(_O,_S,_E) -> {ok,_S}.