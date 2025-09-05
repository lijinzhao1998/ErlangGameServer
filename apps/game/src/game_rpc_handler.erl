%% 游戏服务器RPC处理器
-module(game_rpc_handler).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-behaviour(gen_server).

-record(state, {
    players = #{},
    game_state = running
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % 注册游戏服务到RPC系统
    rpc_api:register_service("game_service", self()),
    
    io:format("游戏RPC处理器已启动并注册为 'game_service'~n"),
    {ok, #state{}}.

%% RPC调用处理

handle_call({get_player, PlayerId}, _From, State) ->
    case maps:find(PlayerId, State#state.players) of
        {ok, PlayerData} ->
            {reply, {ok, PlayerData}, State};
        error ->
            {reply, {error, player_not_found}, State}
    end;

handle_call({create_player, PlayerData}, _From, State) ->
    PlayerId = generate_player_id(),
    NewPlayer = PlayerData#{id => PlayerId, created_at => erlang:system_time(second)},
    NewPlayers = maps:put(PlayerId, NewPlayer, State#state.players),
    {reply, {ok, PlayerId}, State#state{players = NewPlayers}};

handle_call({update_player, PlayerId, Updates}, _From, State) ->
    case maps:find(PlayerId, State#state.players) of
        {ok, PlayerData} ->
            UpdatedPlayer = maps:merge(PlayerData, Updates),
            NewPlayers = State#state.players#{PlayerId => UpdatedPlayer},
            {reply, {ok, UpdatedPlayer}, State#state{players = NewPlayers}};
        error ->
            {reply, {error, player_not_found}, State}
    end;

handle_call({delete_player, PlayerId}, _From, State) ->
    case maps:find(PlayerId, State#state.players) of
        {ok, _PlayerData} ->
            NewPlayers = maps:remove(PlayerId, State#state.players),
            {reply, {ok, deleted}, State#state{players = NewPlayers}};
        error ->
            {reply, {error, player_not_found}, State}
    end;

handle_call({get_all_players}, _From, State) ->
    {reply, {ok, maps:values(State#state.players)}, State};

handle_call({get_game_state}, _From, State) ->
    {reply, {ok, State#state.game_state}, State};

handle_call({set_game_state, NewState}, _From, State) ->
    {reply, {ok, NewState}, State#state{game_state = NewState}};

handle_call({ping}, _From, State) ->
    {reply, {ok, pong}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% RPC通知处理

handle_cast({player_login, PlayerId, PlayerData}, State) ->
    io:format("玩家 ~p 登录游戏~n", [PlayerId]),
    UpdatedPlayerData = maps:put(last_login, erlang:system_time(second), PlayerData),
    NewPlayers = maps:put(PlayerId, UpdatedPlayerData, State#state.players),
    {noreply, State#state{players = NewPlayers}};

handle_cast({player_logout, PlayerId}, State) ->
    io:format("玩家 ~p 退出游戏~n", [PlayerId]),
    case maps:find(PlayerId, State#state.players) of
        {ok, PlayerData} ->
            UpdatedPlayer = PlayerData#{last_logout => erlang:system_time(second)},
            NewPlayers = State#state.players#{PlayerId => UpdatedPlayer},
            {noreply, State#state{players = NewPlayers}};
        error ->
            {noreply, State}
    end;

handle_cast({game_event, EventType, EventData}, State) ->
    io:format("游戏事件: ~p - ~p~n", [EventType, EventData]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    % 注销RPC服务
    rpc_api:unregister_service("game_service"),
    io:format("游戏RPC处理器已停止~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数

generate_player_id() ->
    % 简单的ID生成，实际应该使用更复杂的算法
    erlang:system_time(millisecond) rem 1000000.