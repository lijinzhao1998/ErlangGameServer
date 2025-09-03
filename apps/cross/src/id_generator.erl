-module(id_generator).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([generate_id/1, generate_role_id/0, generate_session_id/0, generate_item_id/0, generate_order_id/0, generate_guild_id/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ID类型定义
-define(ID_TYPES, [
    role_id,        % 玩家角色ID
    session_id,     % 会话ID
    item_id,        % 物品ID
    order_id,       % 订单ID
    guild_id,       % 公会ID
    mail_id,        % 邮件ID
    chat_id,        % 聊天ID
    trade_id,       % 交易ID
    battle_id,      % 战斗ID
    quest_id        % 任务ID
]).

%% 默认起始ID
-define(DEFAULT_START_IDS, #{
    role_id => 1000,      % 玩家ID从1000开始
    session_id => 10000,  % 会话ID从10000开始
    item_id => 20000,     % 物品ID从20000开始
    order_id => 30000,    % 订单ID从30000开始
    guild_id => 40000,    % 公会ID从40000开始
    mail_id => 50000,     % 邮件ID从50000开始
    chat_id => 60000,     % 聊天ID从60000开始
    trade_id => 70000,    % 交易ID从70000开始
    battle_id => 80000,   % 战斗ID从80000开始
    quest_id => 90000     % 任务ID从90000开始
}).

%% 批量生成数量
-define(BATCH_SIZE, 100).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% 生成指定类型的ID
generate_id(Type) ->
    gen_server:call(?MODULE, {generate_id, Type}).

%% 生成玩家角色ID
generate_role_id() ->
    generate_id(role_id).

%% 生成会话ID
generate_session_id() ->
    generate_id(session_id).

%% 生成物品ID
generate_item_id() ->
    generate_id(item_id).

%% 生成订单ID
generate_order_id() ->
    generate_id(order_id).

%% 生成公会ID
generate_guild_id() ->
    generate_id(guild_id).

%% gen_server callbacks
init([]) ->
    %% 初始化ID计数器
    InitialState = init_id_counters(),
    logger:log_info("ID生成器启动成功，初始状态: ~p", [InitialState]),
    {ok, InitialState}.

handle_call({generate_id, Type}, _From, State) ->
    case generate_next_id(Type, State) of
        {ok, NewId, NewState} ->
            logger:log_debug("生成~p类型ID: ~p", [Type, NewId]),
            {reply, {ok, NewId}, NewState};
        {error, Reason} ->
            logger:log_error("生成~p类型ID失败: ~p", [Type, Reason]),
            {reply, {error, Reason}, State}
    end;
handle_call(stop, _From, State) ->
    logger:log_info("ID生成器停止"),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    logger:log_info("ID生成器已停止"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数
init_id_counters() ->
    %% 尝试从数据库加载已存在的ID
    case load_existing_ids() of
        {ok, ExistingIds} ->
            merge_with_defaults(ExistingIds);
        {error, _Reason} ->
            %% 如果加载失败，使用默认值
            logger:log_warning("无法加载现有ID，使用默认值"),
            ?DEFAULT_START_IDS
    end.

load_existing_ids() ->
    %% 从数据库加载各种类型的最大ID
    try
        Ids = maps:fold(fun(Type, _DefaultId, Acc) ->
            case get_max_id_from_db(Type) of
                {ok, MaxId} ->
                    maps:put(Type, MaxId + 1, Acc);
                {error, _} ->
                    Acc
            end
        end, #{}, ?DEFAULT_START_IDS),
        {ok, Ids}
    catch
        _:Reason ->
            logger:log_error("加载现有ID时发生错误: ~p", [Reason]),
            {error, Reason}
    end.

get_max_id_from_db(Type) ->
    %% 根据类型查询数据库中的最大ID
    case Type of
        role_id ->
            query_max_role_id();
        session_id ->
            query_max_session_id();
        item_id ->
            query_max_item_id();
        order_id ->
            query_max_order_id();
        guild_id ->
            query_max_guild_id();
        mail_id ->
            query_max_mail_id();
        chat_id ->
            query_max_chat_id();
        trade_id ->
            query_max_trade_id();
        battle_id ->
            query_max_battle_id();
        quest_id ->
            query_max_quest_id();
        _ ->
            {error, unknown_type}
    end.

%% 查询各种类型的最大ID（模拟实现）
query_max_role_id() ->
    case mdb:query("SELECT MAX(role_id) FROM players") of
        {ok, [[MaxId]]} when is_integer(MaxId) -> {ok, MaxId};
        {ok, [[null]]} -> {ok, 0};
        {ok, []} -> {ok, 0};
        _ -> {error, query_failed}
    end.

query_max_session_id() ->
    case mdb:query("SELECT MAX(CAST(session_id AS INTEGER)) FROM player_sessions") of
        {ok, [[MaxId]]} when is_integer(MaxId) -> {ok, MaxId};
        {ok, [[null]]} -> {ok, 0};
        {ok, []} -> {ok, 0};
        _ -> {error, query_failed}
    end.

query_max_item_id() ->
    %% 模拟查询物品表
    {ok, 0}.

query_max_order_id() ->
    %% 模拟查询订单表
    {ok, 0}.

query_max_guild_id() ->
    %% 模拟查询公会表
    {ok, 0}.

query_max_mail_id() ->
    %% 模拟查询邮件表
    {ok, 0}.

query_max_chat_id() ->
    %% 模拟查询聊天表
    {ok, 0}.

query_max_trade_id() ->
    %% 模拟查询交易表
    {ok, 0}.

query_max_battle_id() ->
    %% 模拟查询战斗表
    {ok, 0}.

query_max_quest_id() ->
    %% 模拟查询任务表
    {ok, 0}.

merge_with_defaults(ExistingIds) ->
    %% 合并现有ID和默认值
    maps:fold(fun(Type, DefaultId, Acc) ->
        case maps:find(Type, ExistingIds) of
            {ok, ExistingId} ->
                maps:put(Type, ExistingId, Acc);
            error ->
                maps:put(Type, DefaultId, Acc)
        end
    end, ?DEFAULT_START_IDS, ExistingIds).

generate_next_id(Type, State) ->
    case maps:find(Type, State) of
        {ok, CurrentId} ->
            NewId = CurrentId + 1,
            NewState = maps:put(Type, NewId, State),
            {ok, NewId, NewState};
        error ->
            {error, unknown_id_type}
    end.

%% 批量生成ID（可选功能）
%% @doc 批量生成指定类型的ID，用于批量操作
%% @spec generate_batch_ids(Type, Count) -> {ok, [Id]} | {error, Reason}
generate_batch_ids(Type, Count) ->
    gen_server:call(?MODULE, {generate_batch_ids, Type, Count}).

%% 重置ID计数器（谨慎使用）
%% @doc 重置指定类型的ID计数器，仅用于维护和调试
%% @spec reset_id_counter(Type, NewValue) -> ok | {error, Reason}
reset_id_counter(Type, NewValue) ->
    gen_server:call(?MODULE, {reset_id_counter, Type, NewValue}).

%% 获取当前ID状态
%% @doc 获取所有ID类型的当前状态，用于监控和调试
%% @spec get_id_status() -> {ok, #{Type => CurrentId}}
get_id_status() ->
    gen_server:call(?MODULE, get_id_status). 