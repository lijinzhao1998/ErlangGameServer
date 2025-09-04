-module(mdb).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([execute/1, execute/2, query/1, query/2]).
-export([get_player/1, create_player/1, update_player/1, delete_player/1]).
-export([get_player_items/1, add_item/2, update_item/2, delete_item/1]).
-export([get_guild/1, create_guild/1, update_guild/1, delete_guild/1]).
-export([force_save/0, get_dirty_data/0, mark_dirty/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DB_FILE, "game_data.db").
-define(SAVE_INTERVAL, 300000). % 5分钟自动存库间隔

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

%% 物品数据操作
get_player_items(RoleId) ->
    gen_server:call(?MODULE, {get_player_items, RoleId}).

add_item(RoleId, ItemData) ->
    gen_server:call(?MODULE, {add_item, RoleId, ItemData}).

update_item(ItemId, ItemData) ->
    gen_server:call(?MODULE, {update_item, ItemId, ItemData}).

delete_item(ItemId) ->
    gen_server:call(?MODULE, {delete_item, ItemId}).

%% 公会数据操作
get_guild(GuildId) ->
    gen_server:call(?MODULE, {get_guild, GuildId}).

create_guild(GuildData) ->
    gen_server:call(?MODULE, {create_guild, GuildData}).

update_guild(GuildData) ->
    gen_server:call(?MODULE, {update_guild, GuildData}).

delete_guild(GuildId) ->
    gen_server:call(?MODULE, {delete_guild, GuildId}).

%% 存库相关接口
force_save() ->
    gen_server:call(?MODULE, force_save).

get_dirty_data() ->
    gen_server:call(?MODULE, get_dirty_data).

mark_dirty(Table, Id) ->
    gen_server:cast(?MODULE, {mark_dirty, Table, Id}).

%% gen_server callbacks
init([]) ->
    %% 初始化数据库连接和表结构
    case init_database() of
        ok ->
            %% 启动定时存库
            erlang:send_after(?SAVE_INTERVAL, self(), auto_save),
            {ok, #{db_initialized => true, dirty_data => #{}}};
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
handle_call({get_player_items, RoleId}, _From, State) ->
    Result = do_get_player_items(RoleId),
    {reply, Result, State};
handle_call({add_item, RoleId, ItemData}, _From, State) ->
    Result = do_add_item(RoleId, ItemData),
    {reply, Result, State};
handle_call({update_item, ItemId, ItemData}, _From, State) ->
    Result = do_update_item(ItemId, ItemData),
    {reply, Result, State};
handle_call({delete_item, ItemId}, _From, State) ->
    Result = do_delete_item(ItemId),
    {reply, Result, State};
handle_call({get_guild, GuildId}, _From, State) ->
    Result = do_get_guild(GuildId),
    {reply, Result, State};
handle_call({create_guild, GuildData}, _From, State) ->
    Result = do_create_guild(GuildData),
    {reply, Result, State};
handle_call({update_guild, GuildData}, _From, State) ->
    Result = do_update_guild(GuildData),
    {reply, Result, State};
handle_call({delete_guild, GuildId}, _From, State) ->
    Result = do_delete_guild(GuildId),
    {reply, Result, State};
handle_call(force_save, _From, State) ->
    {DirtyData, NewState} = maps:take(dirty_data, State),
    case do_force_save(DirtyData) of
        ok ->
            logger:log_info("强制存库成功，处理了 ~p 条脏数据", [maps:size(DirtyData)]),
            {reply, {ok, saved}, NewState#{dirty_data => #{}}};
        {error, Reason} ->
            logger:log_error("强制存库失败: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;
handle_call(get_dirty_data, _From, State) ->
    DirtyData = maps:get(dirty_data, State, #{}),
    {reply, {ok, DirtyData}, State};
handle_call(stop, _From, State) ->
    %% 停止前强制存库
    DirtyData = maps:get(dirty_data, State, #{}),
    case maps:size(DirtyData) of
        0 -> 
            logger:log_info("MDB停止，无脏数据需要存库"),
            {stop, normal, ok, State};
        Size ->
            logger:log_info("MDB停止，强制存库 ~p 条脏数据", [Size]),
            case do_force_save(DirtyData) of
                ok ->
                    logger:log_info("停止前存库成功"),
                    {stop, normal, ok, State};
                {error, Reason} ->
                    logger:log_error("停止前存库失败: ~p", [Reason]),
                    {stop, normal, ok, State}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({mark_dirty, Table, Id}, State) ->
    DirtyData = maps:get(dirty_data, State, #{}),
    TableData = maps:get(Table, DirtyData, #{}),
    NewTableData = TableData#{Id => erlang:system_time(millisecond)},
    NewDirtyData = DirtyData#{Table => NewTableData},
    logger:log_debug("标记数据为脏: ~s:~p", [Table, Id]),
    {noreply, State#{dirty_data => NewDirtyData}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auto_save, State) ->
    DirtyData = maps:get(dirty_data, State, #{}),
    case maps:size(DirtyData) of
        0 ->
            logger:log_debug("定时存库检查：无脏数据"),
            ok;
        Size ->
            logger:log_info("定时存库：处理 ~p 条脏数据", [Size]),
            case do_auto_save(DirtyData) of
                ok ->
                    logger:log_info("定时存库成功"),
                    {noreply, State#{dirty_data => #{}}};
                {error, Reason} ->
                    logger:log_error("定时存库失败: ~p", [Reason]),
                    {noreply, State}
            end
    end,
    %% 重新启动定时器
    erlang:send_after(?SAVE_INTERVAL, self(), auto_save),
    {noreply, State};
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
    
    %% 创建会话表
    CreateSessionTableSql = "
        CREATE TABLE IF NOT EXISTS player_sessions (
            session_id VARCHAR(100) PRIMARY KEY,
            role_id INTEGER NOT NULL,
            username VARCHAR(50) NOT NULL,
            login_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            expire_time TIMESTAMP NOT NULL,
            ip_address VARCHAR(45),
            user_agent TEXT,
            FOREIGN KEY (role_id) REFERENCES players(role_id)
        )
    ",
    
    %% 创建物品表
    CreateItemTableSql = "
        CREATE TABLE IF NOT EXISTS player_items (
            item_id INTEGER PRIMARY KEY,
            role_id INTEGER NOT NULL,
            item_type VARCHAR(50) NOT NULL,
            item_name VARCHAR(100) NOT NULL,
            quantity INTEGER DEFAULT 1,
            quality INTEGER DEFAULT 1,
            create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (role_id) REFERENCES players(role_id)
        )
    ",
    
    %% 创建公会表
    CreateGuildTableSql = "
        CREATE TABLE IF NOT EXISTS guilds (
            guild_id INTEGER PRIMARY KEY,
            guild_name VARCHAR(100) UNIQUE NOT NULL,
            leader_id INTEGER NOT NULL,
            member_count INTEGER DEFAULT 1,
            create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (leader_id) REFERENCES players(role_id)
        )
    ",
    
    %% 执行所有建表语句
    Tables = [
        {players, CreateTableSql},
        {player_sessions, CreateSessionTableSql},
        {player_items, CreateItemTableSql},
        {guilds, CreateGuildTableSql}
    ],
    
    case create_all_tables(Tables) of
        ok -> 
            logger:log_info("数据库初始化成功"),
            ok;
        {error, Reason} -> 
            logger:log_error("数据库初始化失败: ~p", [Reason]),
            {error, Reason}
    end.

create_all_tables([]) ->
    ok;
create_all_tables([{TableName, Sql} | Rest]) ->
    case do_execute(Sql, []) of
        {ok, _} -> 
            logger:log_info("创建表 ~s 成功", [TableName]),
            create_all_tables(Rest);
        {error, Reason} -> 
            logger:log_error("创建表 ~s 失败: ~p", [TableName, Reason]),
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
    %% 使用ID生成器生成新的角色ID
    case id_generator:generate_role_id() of
        {ok, NewRoleId} ->
            %% 更新PlayerData中的role_id
            UpdatedPlayerData = PlayerData#{role_id => NewRoleId},
            
            Sql = "INSERT INTO players (role_id, username, password_hash, nickname) VALUES (?, ?, ?, ?)",
            Params = [
                NewRoleId,
                maps:get(username, UpdatedPlayerData),
                maps:get(password_hash, UpdatedPlayerData),
                maps:get(nickname, UpdatedPlayerData, "")
            ],
            case do_execute(Sql, Params) of
                {ok, _} -> 
                    logger:log_info("创建玩家成功: ~p (RoleId: ~p)", [maps:get(username, UpdatedPlayerData), NewRoleId]),
                    {ok, #{role_id => NewRoleId, username => maps:get(username, UpdatedPlayerData)}};
                {error, Reason} -> 
                    logger:log_error("创建玩家失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:log_error("生成角色ID失败: ~p", [Reason]),
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

%% 物品相关函数
do_get_player_items(RoleId) ->
    Sql = "SELECT * FROM player_items WHERE role_id = ?",
    case do_query(Sql, [RoleId]) of
        {ok, Items} -> 
            logger:log_debug("获取玩家物品成功: ~p, 数量: ~p", [RoleId, length(Items)]),
            {ok, Items};
        {error, Reason} -> 
            logger:log_error("获取玩家物品失败: ~p", [Reason]),
            {error, Reason}
    end.

do_add_item(RoleId, ItemData) ->
    %% 使用ID生成器生成物品ID
    case id_generator:generate_item_id() of
        {ok, ItemId} ->
            Sql = "INSERT INTO player_items (item_id, role_id, item_type, item_name, quantity, quality, level) VALUES (?, ?, ?, ?, ?, ?, ?)",
            Params = [
                ItemId,
                RoleId,
                maps:get(item_type, ItemData),
                maps:get(item_name, ItemData),
                maps:get(quantity, ItemData, 1),
                maps:get(quality, ItemData, 1),
                maps:get(level, ItemData, 1)
            ],
            case do_execute(Sql, Params) of
                {ok, _} -> 
                    logger:log_info("添加物品成功: ~p (ItemId: ~p)", [maps:get(item_name, ItemData), ItemId]),
                    {ok, #{item_id => ItemId, role_id => RoleId}};
                {error, Reason} -> 
                    logger:log_error("添加物品失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:log_error("生成物品ID失败: ~p", [Reason]),
            {error, Reason}
    end.

do_update_item(ItemId, ItemData) ->
    Sql = "UPDATE player_items SET quantity = ?, quality = ?, level = ?, durability = ? WHERE item_id = ?",
    Params = [
        maps:get(quantity, ItemData, 1),
        maps:get(quality, ItemData, 1),
        maps:get(level, ItemData, 1),
        maps:get(durability, ItemData, 100),
        ItemId
    ],
    case do_execute(Sql, Params) of
        {ok, _} -> 
            logger:log_info("更新物品成功: ~p", [ItemId]),
            {ok, updated};
        {error, Reason} -> 
            logger:log_error("更新物品失败: ~p", [Reason]),
            {error, Reason}
    end.

do_delete_item(ItemId) ->
    Sql = "DELETE FROM player_items WHERE item_id = ?",
    case do_execute(Sql, [ItemId]) of
        {ok, _} -> 
            logger:log_info("删除物品成功: ~p", [ItemId]),
            {ok, deleted};
        {error, Reason} -> 
            logger:log_error("删除物品失败: ~p", [Reason]),
            {error, Reason}
    end.

%% 公会相关函数
do_get_guild(GuildId) ->
    Sql = "SELECT * FROM guilds WHERE guild_id = ?",
    case do_query(Sql, [GuildId]) of
        {ok, [Guild]} -> 
            logger:log_debug("获取公会成功: ~p", [GuildId]),
            {ok, Guild};
        {ok, []} -> 
            {error, guild_not_found};
        {error, Reason} -> 
            logger:log_error("获取公会失败: ~p", [Reason]),
            {error, Reason}
    end.

do_create_guild(GuildData) ->
    %% 使用ID生成器生成公会ID
    case id_generator:generate_guild_id() of
        {ok, GuildId} ->
            Sql = "INSERT INTO guilds (guild_id, guild_name, leader_id, member_count, max_members, level, exp, funds, description) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
            Params = [
                GuildId,
                maps:get(guild_name, GuildData),
                maps:get(leader_id, GuildData),
                maps:get(member_count, GuildData, 1),
                maps:get(max_members, GuildData, 100),
                maps:get(level, GuildData, 1),
                maps:get(exp, GuildData, 0),
                maps:get(funds, GuildData, 0),
                maps:get(description, GuildData, "")
            ],
            case do_execute(Sql, Params) of
                {ok, _} -> 
                    logger:log_info("创建公会成功: ~s (GuildId: ~p)", [maps:get(guild_name, GuildData), GuildId]),
                    {ok, #{guild_id => GuildId, guild_name => maps:get(guild_name, GuildData)}};
                {error, Reason} -> 
                    logger:log_error("创建公会失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:log_error("生成公会ID失败: ~p", [Reason]),
            {error, Reason}
    end.

do_update_guild(GuildData) ->
    Sql = "UPDATE guilds SET member_count = ?, level = ?, exp = ?, funds = ?, description = ?, last_active_time = CURRENT_TIMESTAMP WHERE guild_id = ?",
    Params = [
        maps:get(member_count, GuildData),
        maps:get(level, GuildData),
        maps:get(exp, GuildData),
        maps:get(funds, GuildData),
        maps:get(description, GuildData, ""),
        maps:get(guild_id, GuildData)
    ],
    case do_execute(Sql, Params) of
        {ok, _} -> 
            logger:log_info("更新公会成功: ~p", [maps:get(guild_id, GuildData)]),
            {ok, updated};
        {error, Reason} -> 
            logger:log_error("更新公会失败: ~p", [Reason]),
            {error, Reason}
    end.

do_delete_guild(GuildId) ->
    Sql = "DELETE FROM guilds WHERE guild_id = ?",
    case do_execute(Sql, [GuildId]) of
        {ok, _} -> 
            logger:log_info("删除公会成功: ~p", [GuildId]),
            {ok, deleted};
        {error, Reason} -> 
            logger:log_error("删除公会失败: ~p", [Reason]),
            {error, Reason}
    end.

%% 存库相关函数
do_force_save(DirtyData) ->
    %% 强制存库：立即保存所有脏数据
    logger:log_info("开始强制存库，脏数据表数量: ~p", [maps:size(DirtyData)]),
    case save_dirty_data(DirtyData) of
        ok ->
            logger:log_info("强制存库完成"),
            ok;
        {error, Reason} ->
            logger:log_error("强制存库失败: ~p", [Reason]),
            {error, Reason}
    end.

do_auto_save(DirtyData) ->
    %% 自动存库：定时保存脏数据
    logger:log_debug("开始自动存库，脏数据表数量: ~p", [maps:size(DirtyData)]),
    case save_dirty_data(DirtyData) of
        ok ->
            logger:log_debug("自动存库完成"),
            ok;
        {error, Reason} ->
            logger:log_error("自动存库失败: ~p", [Reason]),
            {error, Reason}
    end.

%% 保存脏数据的通用函数
save_dirty_data(DirtyData) ->
    %% 遍历所有脏数据表并保存
    Tables = maps:keys(DirtyData),
    save_dirty_tables(Tables, DirtyData).

save_dirty_tables([], _DirtyData) ->
    ok;
save_dirty_tables([Table | Rest], DirtyData) ->
    TableData = maps:get(Table, DirtyData, #{}),
    case maps:size(TableData) of
        0 ->
            save_dirty_tables(Rest, DirtyData);
        _Size ->
            case save_table_data(Table, TableData) of
                ok ->
                    save_dirty_tables(Rest, DirtyData);
                {error, Reason} ->
                    logger:log_error("保存表 ~s 数据失败: ~p", [Table, Reason]),
                    {error, Reason}
            end
    end.

save_table_data(Table, TableData) ->
    %% 根据表名选择不同的保存策略
    case Table of
        players ->
            save_players_data(TableData);
        player_items ->
            save_items_data(TableData);
        guilds ->
            save_guilds_data(TableData);
        _Other ->
            logger:log_warning("未知的脏数据表: ~s", [Table]),
            ok
    end.

save_players_data(TableData) ->
    %% 保存玩家数据
    PlayerIds = maps:keys(TableData),
    save_players_by_ids(PlayerIds).

save_players_by_ids([]) ->
    ok;
save_players_by_ids([PlayerId | Rest]) ->
    %% 这里应该从内存缓存中获取玩家数据并保存到数据库
    %% 由于当前是模拟实现，这里只记录日志
    logger:log_debug("保存玩家数据: ~p", [PlayerId]),
    save_players_by_ids(Rest).

save_items_data(TableData) ->
    %% 保存物品数据
    ItemIds = maps:keys(TableData),
    save_items_by_ids(ItemIds).

save_items_by_ids([]) ->
    ok;
save_items_by_ids([ItemId | Rest]) ->
    %% 这里应该从内存缓存中获取物品数据并保存到数据库
    %% 由于当前是模拟实现，这里只记录日志
    logger:log_debug("保存物品数据: ~p", [ItemId]),
    save_items_by_ids(Rest).

save_guilds_data(TableData) ->
    %% 保存公会数据
    GuildIds = maps:keys(TableData),
    save_guilds_by_ids(GuildIds).

save_guilds_by_ids([]) ->
    ok;
save_guilds_by_ids([GuildId | Rest]) ->
    %% 这里应该从内存缓存中获取公会数据并保存到数据库
    %% 由于当前是模拟实现，这里只记录日志
    logger:log_debug("保存公会数据: ~p", [GuildId]),
    save_guilds_by_ids(Rest). 