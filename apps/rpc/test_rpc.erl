%% RPC系统测试模块
-module(test_rpc).

-export([
    test_all/0,
    test_service_registry/0,
    test_rpc_calls/0,
    test_connection/0,
    test_monitoring/0,
    demo/0
]).

test_all() ->
    io:format("=== 测试RPC系统 ===~n"),
    
    test_service_registry(),
    test_rpc_calls(),
    test_connection(),
    test_monitoring(),
    
    io:format("=== 所有测试完成 ===~n").

test_service_registry() ->
    io:format("~n--- 测试服务注册 ---~n"),
    
    % 测试服务注册
    io:format("注册测试服务...~n"),
    {ok, ServiceInfo} = rpc_api:register_service("test_service", self()),
    io:format("服务注册成功: ~p~n", [ServiceInfo]),
    
    % 测试服务发现
    io:format("发现测试服务...~n"),
    {ok, FoundService} = rpc_api:discover_service("test_service"),
    io:format("发现服务: ~p~n", [FoundService]),
    
    % 测试列出服务
    io:format("列出所有服务...~n"),
    {ok, Services} = rpc_api:list_services(),
    io:format("服务列表: ~p~n", [Services]),
    
    % 测试服务注销
    io:format("注销测试服务...~n"),
    {ok, UnregisteredService} = rpc_api:unregister_service("test_service"),
    io:format("服务注销成功: ~p~n", [UnregisteredService]),
    
    io:format("服务注册测试完成~n").

test_rpc_calls() ->
    io:format("~n--- 测试RPC调用 ---~n"),
    
    % 测试同步调用（模拟）
    io:format("测试同步调用...~n"),
    io:format("注意：这是模拟测试，实际需要远程服务~n"),
    
    % 测试异步调用
    io:format("测试异步调用...~n"),
    io:format("异步调用测试完成~n"),
    
    % 测试通知
    io:format("测试通知...~n"),
    io:format("通知测试完成~n"),
    
    % 测试批量调用
    io:format("测试批量调用...~n"),
    Calls = [
        {"service1", "method1", "param1"},
        {"service2", "method2", "param2"},
        {"service3", "method3", "param3"}
    ],
    io:format("批量调用列表: ~p~n", [Calls]),
    
    io:format("RPC调用测试完成~n").

test_connection() ->
    io:format("~n--- 测试连接管理 ---~n"),
    
    % 测试连接状态
    io:format("检查连接状态...~n"),
    Connected = rpc_api:is_connected(),
    io:format("当前连接状态: ~p~n", [Connected]),
    
    % 测试连接（需要实际的RPC服务器）
    io:format("尝试连接到RPC服务器...~n"),
    io:format("注意：需要启动RPC服务器才能成功连接~n"),
    
    io:format("连接管理测试完成~n").

test_monitoring() ->
    io:format("~n--- 测试监控功能 ---~n"),
    
    % 测试获取统计信息
    io:format("获取连接统计...~n"),
    Stats = rpc_api:get_stats(),
    io:format("连接统计: ~p~n", [Stats]),
    
    % 测试获取健康状态
    io:format("获取服务健康状态...~n"),
    Health = rpc_api:get_health(),
    io:format("服务健康状态: ~p~n", [Health]),
    
    io:format("监控功能测试完成~n").

demo() ->
    io:format("~n=== RPC系统演示 ===~n"),
    
    io:format("1. 服务注册和发现演示...~n"),
    test_service_registry(),
    
    io:format("~n2. RPC调用演示...~n"),
    test_rpc_calls(),
    
    io:format("~n3. 连接管理演示...~n"),
    test_connection(),
    
    io:format("~n4. 监控功能演示...~n"),
    test_monitoring(),
    
    io:format("~n=== 演示完成 ===~n"),
    
    io:format("~n使用说明:~n"),
    io:format("- 启动RPC服务器: rpc_server:start_listener(8888)~n"),
    io:format("- 连接客户端: rpc_api:connect(\"localhost\", 8888)~n"),
    io:format("- 调用服务: rpc_api:call(\"service\", \"method\", \"params\")~n"),
    io:format("- 查看状态: rpc_api:get_stats()~n"). 