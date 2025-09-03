#!/bin/bash

echo "启动RPC系统演示..."

# 检查Erlang是否安装
if ! command -v erl &> /dev/null; then
    echo "错误: 未找到Erlang，请先安装Erlang/OTP"
    exit 1
fi

echo "正在启动RPC系统演示..."
echo.

# 启动Erlang shell并加载RPC系统
erl -pa apps/*/ebin -eval "
    % 启动RPC应用
    application:start(rpc),
    
    % 启动RPC服务器监听器
    rpc_server:start_listener(8888),
    
    % 等待启动完成
    timer:sleep(2000),
    
    % 显示启动信息
    io:format('~n=== RPC系统已启动 ===~n'),
    io:format('监听端口: 8888~n'),
    io:format('服务注册表: 已启动~n'),
    io:format('监控系统: 已启动~n'),
    io:format('~n=== 演示命令 ===~n'),
    io:format('1. 运行测试: test_rpc:test_all().~n'),
    io:format('2. 运行演示: test_rpc:demo().~n'),
    io:format('3. 查看服务: rpc_api:list_services().~n'),
    io:format('4. 查看状态: rpc_api:get_stats().~n'),
    io:format('~n按 Ctrl+C 退出~n'),
    
    % 等待用户输入
    receive _ -> ok end
" 