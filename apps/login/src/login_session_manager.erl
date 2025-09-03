%% Simple session/token manager using ETS for demonstration.
-module(login_session_manager).
-behaviour(gen_server).

-export([start_link/0, create_token/1, lookup/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TABLE, login_tokens).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Table = ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}]),
    {ok, #{table => Table}}.

create_token(User) ->
    gen_server:call(?MODULE, {create, User}).

lookup(Token) ->
    gen_server:call(?MODULE, {lookup, Token}).

handle_call({create, User}, _From, State = #{table := Table}) ->
    Token = crypto:strong_rand_bytes(12) |> base64:encode_to_string(),
    ets:insert(Table, {Token, User, erlang:system_time(seconds)}),
    {reply, {ok, Token}, State};
handle_call({lookup, Token}, _From, State = #{table := Table}) ->
    case ets:lookup(Table, Token) of
        [{_T, User, _Ts}] -> {reply, {ok, User}, State};
        [] -> {reply, {error, not_found}, State}
    end;
handle_call(_Other, _From, State) -> {reply, {error, bad_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.