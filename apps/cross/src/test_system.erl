-module(test_system).
-export([test_all/0, test_logger/0, test_mdb/0, test_auth/0, test_player/0]).

%% 测试所有功能
test_all() ->
    io:format("=== 开始测试所有系统功能 ===~n"),
    
    test_logger(),
    test_mdb(),
    test_auth(),
    test_player(),
    
    io:format("=== 所有测试完成 ===~n").

%% 测试日志系统
test_logger() ->
    io:format("~n--- 测试日志系统 ---~n"),
    
    logger:log_info("这是一条信息日志"),
    logger:log_warning("这是一条警告日志"),
    logger:log_error("这是一条错误日志"),
    logger:log_debug("这是一条调试日志"),
    
    logger:log(info, ?MODULE, "带模块名的日志"),
    logger:log(error, ?MODULE, "带参数的日志: ~p", [test_data]),
    
    io:format("日志测试完成，请检查logs目录~n").

%% 测试MDB数据库
test_mdb() ->
    io:format("~n--- 测试MDB数据库 ---~n"),
    
    %% 测试查询
    case mdb:query("SELECT * FROM players LIMIT 5") of
        {ok, Result} ->
            io:format("查询结果: ~p~n", [Result]);
        {error, Reason} ->
            io:format("查询失败: ~p~n", [Reason])
    end,
    
    %% 测试获取玩家
    case mdb:get_player(1000) of
        {ok, Player} ->
            io:format("获取玩家1000成功: ~p~n", [Player]);
        {error, Reason} ->
            io:format("获取玩家1000失败: ~p~n", [Reason])
    end,
    
    io:format("MDB测试完成~n").

%% 测试玩家认证
test_auth() ->
    io:format("~n--- 测试玩家认证 ---~n"),
    
    %% 测试登录
    case player_auth:login("admin", "123456") of
        {ok, #{session_id := SessionId, role_id := RoleId}} ->
            io:format("登录成功: SessionId=~s, RoleId=~p~n", [SessionId, RoleId]);
        {error, Reason} ->
            io:format("登录失败: ~p~n", [Reason])
    end,
    
    %% 测试注册
    case player_auth:register("newuser", "newpass", "新用户") of
        {ok, #{role_id := NewRoleId, username := Username}} ->
            io:format("注册成功: ~s (RoleId: ~p)~n", [Username, NewRoleId]);
        {error, Reason} ->
            io:format("注册失败: ~p~n", [Reason])
    end,
    
    io:format("认证测试完成~n").

%% 测试玩家进程
test_player() ->
    io:format("~n--- 测试玩家进程 ---~n"),
    
    %% 启动玩家进程
    case game_player:start_link(1000) of
        {ok, Pid} ->
            io:format("玩家进程启动成功: ~p~n", [Pid]);
        {error, Reason} ->
            io:format("玩家进程启动失败: ~p~n", [Reason])
    end,
    
    %% 测试获取状态
    case game_player:get_state(1000) of
        {ok, State} ->
            io:format("玩家状态: ~p~n", [State]);
        {error, Reason} ->
            io:format("获取状态失败: ~p~n", [Reason])
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
        {error, Reason} ->
            io:format("获取更新状态失败: ~p~n", [Reason])
    end,
    
    %% 手动保存状态
    case game_player:save_state(1000) of
        {ok, saved} ->
            io:format("手动保存成功~n");
        {error, Reason} ->
            io:format("手动保存失败: ~p~n", [Reason])
    end,
    
    io:format("玩家进程测试完成~n").

%% 演示函数
demo() ->
    io:format("~n=== 系统演示 ===~n"),
    io:format("1. 测试日志系统...~n"),
    test_logger(),
    
    io:format("~n2. 测试数据库...~n"),
    test_mdb(),
    
    io:format("~n3. 测试认证...~n"),
    test_auth(),
    
    io:format("~n4. 测试玩家进程...~n"),
    test_player(),
    
    io:format("~n=== 演示完成 ===~n"). 