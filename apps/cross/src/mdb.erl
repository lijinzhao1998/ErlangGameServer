-module(mdb).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([execute/1, execute/2, query/1, query/2]).
-export([get_player/1, create_player/1, update_player/1, delete_player/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DB_FILE, "game_data.db").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% 数据库操作接口
execute(Sql) ->
    execute(Sql, []).

execute(Sql, Params) ->
    gen_server:call(?MODULE, {execute, Sql, Params}).

query(Sql) ->
    query(Sql, []).

query(Sql, Params) ->
    gen_server:call(?MODULE, {query, Sql, Params}).

%% 玩家数据操作
get_player(RoleId) ->
    gen_server:call(?MODULE, {get_player, RoleId}).

create_player(PlayerData) ->
    gen_server:call(?MODULE, {create_player, PlayerData}).

update_player(PlayerData) ->
    gen_server:call(?MODULE, {update_player, PlayerData}).

delete_player(RoleId) ->
    gen_server:call(?MODULE, {delete_player, RoleId}).

%% gen_server callbacks
init([]) ->
    %% 初始化数据库连接和表结构
    case init_database() of
        ok ->
            {ok, #{db_initialized => true}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({execute, Sql, Params}, _From, State) ->
    Result = do_execute(Sql, Params),
    {reply, Result, State};
handle_call({query, Sql, Params}, _From, State) ->
    Result = do_query(Sql, Params),
    {reply, Result, State};
handle_call({get_player, RoleId}, _From, State) ->
    Result = do_get_player(RoleId),
    {reply, Result, State};
handle_call({create_player, PlayerData}, _From, State) ->
    Result = do_create_player(PlayerData),
    {reply, Result, State};
handle_call({update_player, PlayerData}, _From, State) ->
    Result = do_update_player(PlayerData),
    {reply, Result, State};
handle_call({delete_player, RoleId}, _From, State) ->
    Result = do_delete_player(RoleId),
    {reply, Result, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数
init_database() ->
    %% 这里应该使用实际的数据库驱动，这里用文件模拟
    %% 创建玩家表结构
    CreateTableSql = "
        CREATE TABLE IF NOT EXISTS players (
            role_id INTEGER PRIMARY KEY,
            username VARCHAR(50) UNIQUE NOT NULL,
            password_hash VARCHAR(255) NOT NULL,
            nickname VARCHAR(50),
            level INTEGER DEFAULT 1,
            exp INTEGER DEFAULT 0,
            gold INTEGER DEFAULT 0,
            create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            last_login_time TIMESTAMP,
            status INTEGER DEFAULT 1
        )
    ",
    
    case do_execute(CreateTableSql, []) of
        {ok, _} -> 
            logger:log_info("数据库初始化成功"),
            ok;
        {error, Reason} -> 
            logger:log_error("数据库初始化失败: ~p", [Reason]),
            {error, Reason}
    end.

do_execute(Sql, Params) ->
    %% 这里应该使用实际的数据库驱动执行SQL
    %% 现在用模拟实现
    logger:log_debug("执行SQL: ~s 参数: ~p", [Sql, Params]),
    {ok, "executed"}.

do_query(Sql, Params) ->
    %% 这里应该使用实际的数据库驱动查询数据
    %% 现在用模拟实现
    logger:log_debug("查询SQL: ~s 参数: ~p", [Sql, Params]),
    {ok, []}.

do_get_player(RoleId) ->
    Sql = "SELECT * FROM players WHERE role_id = ?",
    case do_query(Sql, [RoleId]) of
        {ok, []} -> {error, player_not_found};
        {ok, [Player]} -> {ok, Player};
        {error, Reason} -> {error, Reason}
    end.

do_create_player(PlayerData) ->
    Sql = "INSERT INTO players (role_id, username, password_hash, nickname) VALUES (?, ?, ?, ?)",
    Params = [
        maps:get(role_id, PlayerData),
        maps:get(username, PlayerData),
        maps:get(password_hash, PlayerData),
        maps:get(nickname, PlayerData, "")
    ],
    case do_execute(Sql, Params) of
        {ok, _} -> 
            logger:log_info("创建玩家成功: ~p", [maps:get(role_id, PlayerData)]),
            {ok, created};
        {error, Reason} -> 
            logger:log_error("创建玩家失败: ~p", [Reason]),
            {error, Reason}
    end.

do_update_player(PlayerData) ->
    Sql = "UPDATE players SET nickname = ?, level = ?, exp = ?, gold = ?, last_login_time = CURRENT_TIMESTAMP WHERE role_id = ?",
    Params = [
        maps:get(nickname, PlayerData),
        maps:get(level, PlayerData, 1),
        maps:get(exp, PlayerData, 0),
        maps:get(gold, PlayerData, 0),
        maps:get(role_id, PlayerData)
    ],
    case do_execute(Sql, Params) of
        {ok, _} -> 
            logger:log_info("更新玩家成功: ~p", [maps:get(role_id, PlayerData)]),
            {ok, updated};
        {error, Reason} -> 
            logger:log_error("更新玩家失败: ~p", [Reason]),
            {error, Reason}
    end.

do_delete_player(RoleId) ->
    Sql = "DELETE FROM players WHERE role_id = ?",
    case do_execute(Sql, [RoleId]) of
        {ok, _} -> 
            logger:log_info("删除玩家成功: ~p", [RoleId]),
            {ok, deleted};
        {error, Reason} -> 
            logger:log_error("删除玩家失败: ~p", [Reason]),
            {error, Reason}
    end. 