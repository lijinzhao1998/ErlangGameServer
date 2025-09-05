-module(test_system).
-export([test_all/0, test_logger/0, test_id_generator/0, test_mdb/0, test_items_and_guilds/0, test_auth/0, test_player/0, demo/0, start_services/0]).

%% 测试所有功能
test_all() ->
    io:format("=== 开始测试所有系统功能 ===~n"),
    
    %% 首先启动必要的服务
    start_services(),
    
    test_logger(),
    test_id_generator(),
    test_mdb(),
    test_items_and_guilds(),
    test_auth(),
    test_player(),
    
    io:format("=== 所有测试完成 ===~n").

%% 测试日志系统
test_logger() ->
    io:format("~n--- 测试日志系统 ---~n"),
    
    custom_logger:log_info("这是一条信息日志"),
    custom_logger:log_warning("这是一条警告日志"),
    custom_logger:log_error("这是一条错误日志"),
    custom_logger:log_debug("这是一条调试日志"),
    
    custom_logger:log(info, ?MODULE, "带模块名的日志"),
    custom_logger:log(error, ?MODULE, "带参数的日志: ~p", [test_data]),
    
    %% 测试带堆栈跟踪的日志
    try
        throw(test_exception)
    catch
        _:Reason:StackTrace ->
            custom_logger:log_with_stack(error, ?MODULE, "捕获到异常: ~p", [Reason], StackTrace)
    end,
    
    %% 测试带参数的堆栈日志
    custom_logger:log_with_stack(warning, ?MODULE, "警告消息: ~p", [warning_data], []),
    
    io:format("日志测试完成，请检查logs目录和控制台输出~n").

%% 测试ID生成器
test_id_generator() ->
    io:format("~n--- 测试ID生成器 ---~n"),
    
    %% 测试生成各种类型的ID
    case id_generator:generate_role_id() of
        {ok, RoleId} ->
            io:format("生成角色ID成功: ~p~n", [RoleId]);
        {error, Reason1} ->
            io:format("生成角色ID失败: ~p~n", [Reason1])
    end,
    
    case id_generator:generate_session_id() of
        {ok, SessionId} ->
            io:format("生成会话ID成功: ~p~n", [SessionId]);
        {error, Reason2} ->
            io:format("生成会话ID失败: ~p~n", [Reason2])
    end,
    
    case id_generator:generate_item_id() of
        {ok, ItemId} ->
            io:format("生成物品ID成功: ~p~n", [ItemId]);
        {error, Reason3} ->
            io:format("生成物品ID失败: ~p~n", [Reason3])
    end,
    
    case id_generator:generate_order_id() of
        {ok, OrderId} ->
            io:format("生成订单ID成功: ~p~n", [OrderId]);
        {error, Reason4} ->
            io:format("生成订单ID失败: ~p~n", [Reason4])
    end,
    
    %% 测试生成公会ID
    case id_generator:generate_guild_id() of
        {ok, GuildId} ->
            io:format("生成公会ID成功: ~p~n", [GuildId]);
        {error, Reason5} ->
            io:format("生成公会ID失败: ~p~n", [Reason5])
    end,
    
    %% 测试批量生成
    case id_generator:generate_id(role_id) of
        {ok, RoleId2} ->
            io:format("使用通用接口生成角色ID成功: ~p~n", [RoleId2]);
        {error, Reason6} ->
            io:format("使用通用接口生成角色ID失败: ~p~n", [Reason6])
    end,
    
    io:format("ID生成器测试完成~n").

%% 测试MDB数据库
test_mdb() ->
    io:format("~n--- 测试MDB数据库 ---~n"),
    
    %% 测试查询
    case mdb:query("SELECT * FROM players LIMIT 5") of
        {ok, Result} ->
            io:format("查询结果: ~p~n", [Result]);
        {error, Reason7} ->
            io:format("查询失败: ~p~n", [Reason7])
    end,
    
    %% 测试获取玩家
    case mdb:get_player(1000) of
        {ok, Player} ->
            io:format("获取玩家1000成功: ~p~n", [Player]);
        {error, Reason8} ->
            io:format("获取玩家1000失败: ~p~n", [Reason8])
    end,
    
    io:format("MDB测试完成~n").

%% 测试物品和公会
test_items_and_guilds() ->
    io:format("~n--- 测试物品和公会系统 ---~n"),
    
    %% 测试添加物品
    ItemData = #{
        item_type => "weapon",
        item_name => "测试武器",
        quantity => 1,
        quality => 3,
        level => 5
    },
    
    case mdb:add_item(1000, ItemData) of
        {ok, #{item_id := ItemId}} ->
            io:format("添加物品成功: ~p~n", [ItemId]);
        {error, Reason9} ->
            io:format("添加物品失败: ~p~n", [Reason9])
    end,
    
    %% 测试获取玩家物品
    case mdb:get_player_items(1000) of
        {ok, Items} ->
            io:format("获取玩家物品成功，数量: ~p~n", [length(Items)]);
        {error, Reason10} ->
            io:format("获取玩家物品失败: ~p~n", [Reason10])
    end,
    
    %% 测试创建公会
    GuildData = #{
        guild_name => "测试公会",
        leader_id => 1000,
        member_count => 1,
        max_members => 50,
        level => 1,
        exp => 0,
        funds => 1000,
        description => "这是一个测试公会"
    },
    
    case mdb:create_guild(GuildData) of
        {ok, #{guild_id := GuildId}} ->
            io:format("创建公会成功: ~p~n", [GuildId]);
        {error, Reason11} ->
            io:format("创建公会失败: ~p~n", [Reason11])
    end,
    
    %% 测试获取公会信息
    case mdb:get_guild(40001) of
        {ok, Guild} ->
            io:format("获取公会信息成功: ~p~n", [Guild]);
        {error, Reason12} ->
            io:format("获取公会信息失败: ~p~n", [Reason12])
    end,
    
    io:format("物品和公会测试完成~n").

%% 测试玩家认证
test_auth() ->
    io:format("~n--- 测试玩家认证 ---~n"),
    
    %% 测试登录
    case player_auth:login("admin", "123456") of
        {ok, #{session_id := SessionId, role_id := RoleId}} ->
            io:format("登录成功: SessionId=~s, RoleId=~p~n", [SessionId, RoleId]);
        {error, Reason13} ->
            io:format("登录失败: ~p~n", [Reason13])
    end,
    
    %% 测试注册
    case player_auth:register("newuser", "newpass", "新用户") of
        {ok, #{role_id := NewRoleId, username := Username}} ->
            io:format("注册成功: ~s (RoleId: ~p)~n", [Username, NewRoleId]);
        {error, Reason14} ->
            io:format("注册失败: ~p~n", [Reason14])
    end,
    
    io:format("认证测试完成~n").

%% 测试玩家进程
test_player() ->
    io:format("~n--- 测试玩家进程 ---~n"),
    
    %% 启动玩家进程
    case game_player:start_link(1000) of
        {ok, Pid} ->
            io:format("玩家进程启动成功: ~p~n", [Pid]);
        {error, Reason15} ->
            io:format("玩家进程启动失败: ~p~n", [Reason15])
    end,
    
    %% 测试获取状态
    case game_player:get_state(1000) of
        {ok, State} ->
            io:format("玩家状态: ~p~n", [State]);
        {error, Reason16} ->
            io:format("获取状态失败: ~p~n", [Reason16])
    end,
    
    %% 测试添加经验
    game_player:add_exp(1000, 500),
    io:format("已添加500经验~n"),
    
    %% 测试添加金币
    game_player:add_gold(1000, 1000),
    io:format("已添加1000金币~n"),
    
    %% 测试设置昵称
    game_player:set_nickname(1000, "新昵称"),
    io:format("已设置新昵称~n"),
    
    %% 再次获取状态查看变化
    case game_player:get_state(1000) of
        {ok, NewState} ->
            io:format("更新后的状态: ~p~n", [NewState]);
        {error, Reason17} ->
            io:format("获取更新状态失败: ~p~n", [Reason17])
    end,
    
    %% 手动保存状态
    case game_player:save_state(1000) of
        {ok, saved} ->
            io:format("手动保存成功~n");
        {error, Reason18} ->
            io:format("手动保存失败: ~p~n", [Reason18])
    end,
    
    io:format("玩家进程测试完成~n").

%% 启动必要的服务
start_services() ->
    io:format("~n--- 启动系统服务 ---~n"),
    
    %% 启动日志系统
    case whereis(custom_logger) of
        undefined ->
            case custom_logger:start_link() of
                {ok, _} ->
                    io:format("日志系统启动成功~n");
                {error, {already_started, _}} ->
                    io:format("日志系统已经运行~n");
                {error, Reason1} ->
                    io:format("日志系统启动失败: ~p~n", [Reason1])
            end;
        _ ->
            io:format("日志系统已经运行~n")
    end,
    
    %% 启动ID生成器
    case whereis(id_generator) of
        undefined ->
            case id_generator:start_link() of
                {ok, _} ->
                    io:format("ID生成器启动成功~n");
                {error, {already_started, _}} ->
                    io:format("ID生成器已经运行~n");
                {error, Reason2} ->
                    io:format("ID生成器启动失败: ~p~n", [Reason2])
            end;
        _ ->
            io:format("ID生成器已经运行~n")
    end,
    
    io:format("服务启动完成~n").

%% 演示函数
demo() ->
    io:format("~n=== 系统演示 ===~n"),
    
    %% 首先启动必要的服务
    start_services(),
    
    io:format("1. 测试日志系统...~n"),
    test_logger(),
    
    io:format("~n2. 测试ID生成器...~n"),
    test_id_generator(),
    
    io:format("~n3. 测试数据库...~n"),
    test_mdb(),
    
    io:format("~n4. 测试物品和公会...~n"),
    test_items_and_guilds(),
    
    io:format("~n5. 测试认证...~n"),
    test_auth(),
    
    io:format("~n6. 测试玩家进程...~n"),
    test_player(),
    
    io:format("~n=== 演示完成 ===~n"). 