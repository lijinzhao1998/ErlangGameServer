%% 增强的玩家进程模块，支持数据库加载和状态管理
-module(game_player).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([get_state/1, update_state/2, save_state/1]).
-export([add_exp/2, add_gold/2, set_nickname/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SAVE_INTERVAL, 300000). % 5分钟自动保存
-define(MAX_EXP_PER_LEVEL, 1000).

start_link(RoleId) ->
    gen_server:start_link({local, {?MODULE, RoleId}}, ?MODULE, RoleId, []).

stop(RoleId) ->
    gen_server:call({?MODULE, RoleId}, stop).

%% 玩家状态查询和更新接口
get_state(RoleId) ->
    gen_server:call({?MODULE, RoleId}, get_state).

update_state(RoleId, Updates) ->
    gen_server:cast({?MODULE, RoleId}, {update_state, Updates}).

save_state(RoleId) ->
    gen_server:call({?MODULE, RoleId}, save_state).

%% 玩家属性操作接口
add_exp(RoleId, Exp) ->
    gen_server:cast({?MODULE, RoleId}, {add_exp, Exp}).

add_gold(RoleId, Gold) ->
    gen_server:cast({?MODULE, RoleId}, {add_gold, Gold}).

set_nickname(RoleId, Nickname) ->
    gen_server:cast({?MODULE, RoleId}, {set_nickname, Nickname}).

%% gen_server callbacks
init(RoleId) ->
    logger:log_info("启动玩家进程: ~p", [RoleId]),
    
    %% 从数据库加载玩家数据
    case mdb:get_player(RoleId) of
        {ok, PlayerData} ->
            State = #{
                role_id => RoleId,
                username => maps:get(username, PlayerData, ""),
                nickname => maps:get(nickname, PlayerData, ""),
                level => maps:get(level, PlayerData, 1),
                exp => maps:get(exp, PlayerData, 0),
                gold => maps:get(gold, PlayerData, 0),
                create_time => maps:get(create_time, PlayerData, erlang:system_time(seconds)),
                last_login_time => erlang:system_time(seconds),
                status => maps:get(status, PlayerData, 1),
                last_save_time => erlang:system_time(seconds),
                dirty => false
            },
            
            %% 设置自动保存定时器
            erlang:send_after(?SAVE_INTERVAL, self(), auto_save),
            
            {ok, State};
        {error, player_not_found} ->
            logger:log_error("玩家数据不存在: ~p", [RoleId]),
            {stop, player_not_found};
        {error, Reason} ->
            logger:log_error("加载玩家数据失败: ~p, 原因: ~p", [RoleId, Reason]),
            {stop, Reason}
    end.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(save_state, _From, State) ->
    case do_save_state(State) of
        {ok, _} ->
            NewState = State#{dirty => false, last_save_time => erlang:system_time(seconds)},
            {reply, {ok, saved}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(stop, _From, State) ->
    %% 停止前保存状态
    do_save_state(State),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_state, Updates}, State) ->
    NewState = maps:merge(State, Updates),
    NewStateWithDirty = NewState#{dirty => true},
    logger:log_debug("更新玩家状态: ~p, 更新: ~p", [maps:get(role_id, State), Updates]),
    {noreply, NewStateWithDirty};
handle_cast({add_exp, Exp}, State) ->
    CurrentExp = maps:get(exp, State),
    CurrentLevel = maps:get(level, State),
    NewExp = CurrentExp + Exp,
    
    %% 检查是否升级
    {NewLevel, FinalExp} = check_level_up(CurrentLevel, NewExp),
    
    NewState = State#{
        exp => FinalExp,
        level => NewLevel,
        dirty => true
    },
    
    if
        NewLevel > CurrentLevel ->
            logger:log_info("玩家 ~p 升级到 ~p 级", [maps:get(role_id, State), NewLevel]);
        true ->
            ok
    end,
    
    {noreply, NewState};
handle_cast({add_gold, Gold}, State) ->
    CurrentGold = maps:get(gold, State),
    NewGold = CurrentGold + Gold,
    NewState = State#{
        gold => NewGold,
        dirty => true
    },
    logger:log_debug("玩家 ~p 获得金币: ~p, 当前: ~p", [maps:get(role_id, State), Gold, NewGold]),
    {noreply, NewState};
handle_cast({set_nickname, Nickname}, State) ->
    NewState = State#{
        nickname => Nickname,
        dirty => true
    },
    logger:log_info("玩家 ~p 设置昵称: ~s", [maps:get(role_id, State), Nickname]),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auto_save, State) ->
    case maps:get(dirty, State, false) of
        true ->
            case do_save_state(State) of
                {ok, _} ->
                    logger:log_debug("玩家 ~p 自动保存成功", [maps:get(role_id, State)]),
                    NewState = State#{dirty => false, last_save_time => erlang:system_time(seconds)};
                {error, Reason} ->
                    logger:log_warning("玩家 ~p 自动保存失败: ~p", [maps:get(role_id, State), Reason]),
                    NewState = State
            end;
        false ->
            NewState = State
    end,
    
    %% 重新设置定时器
    erlang:send_after(?SAVE_INTERVAL, self(), auto_save),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    %% 进程终止时保存状态
    case maps:get(dirty, State, false) of
        true ->
            do_save_state(State);
        false ->
            ok
    end,
    logger:log_info("玩家进程终止: ~p, 原因: ~p", [maps:get(role_id, State), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数
do_save_state(State) ->
    PlayerData = #{
        role_id => maps:get(role_id, State),
        nickname => maps:get(nickname, State),
        level => maps:get(level, State),
        exp => maps:get(exp, State),
        gold => maps:get(gold, State),
        last_login_time => maps:get(last_login_time, State)
    },
    mdb:update_player(PlayerData).

check_level_up(CurrentLevel, TotalExp) ->
    check_level_up(CurrentLevel, TotalExp, 0).

check_level_up(Level, Exp, AccumulatedExp) ->
    RequiredExp = Level * ?MAX_EXP_PER_LEVEL,
    if
        Exp >= RequiredExp ->
            check_level_up(Level + 1, Exp - RequiredExp, AccumulatedExp + RequiredExp);
        true ->
            {Level, Exp}
    end.