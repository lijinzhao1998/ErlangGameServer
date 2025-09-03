#!/bin/bash

echo "启动完整的游戏服务器集成系统..."
echo "包括: RPC系统 + HTTPS API + 游戏服务器"
echo.

# 检查Erlang是否安装
if ! command -v erl &> /dev/null; then
    echo "错误: 未找到Erlang，请先安装Erlang/OTP"
    exit 1
fi

echo "正在启动集成系统..."
echo.

# 启动Erlang shell并加载完整系统
erl -pa apps/*/ebin -eval "
    % 1. 启动RPC系统
    io:format('~n=== 启动RPC系统 ===~n'),
    application:start(rpc),
    rpc_server:start_listener(8888),
    timer:sleep(1000),
    
    % 2. 启动HTTPS API系统
    io:format('~n=== 启动HTTPS API系统 ===~n'),
    application:start(https_api),
    timer:sleep(1000),
    
    % 3. 启动游戏服务器
    io:format('~n=== 启动游戏服务器 ===~n'),
    application:start(game),
    timer:sleep(1000),
    
    % 4. 启动游戏RPC处理器
    io:format('~n=== 启动游戏RPC处理器 ===~n'),
    game_rpc_handler:start_link(),
    timer:sleep(1000),
    
    % 5. 显示系统状态
    io:format('~n=== 系统启动完成 ===~n'),
    io:format('RPC服务器: 端口 8888~n'),
    io:format('HTTPS API: 端口 8443~n'),
    io:format('游戏服务器: 已启动~n'),
    io:format('游戏RPC处理器: 已启动~n'),
    
    % 6. 显示可用命令
    io:format('~n=== 可用命令 ===~n'),
    io:format('RPC测试: test_rpc:demo().~n'),
    io:format('查看服务: rpc_api:list_services().~n'),
    io:format('查看状态: rpc_api:get_stats().~n'),
    io:format('游戏状态: game_rpc_handler:get_game_state().~n'),
    io:format('创建玩家: game_rpc_handler:create_player(#{name => \"测试玩家\"}).~n'),
    io:format('~n按 Ctrl+C 退出~n'),
    
    % 等待用户输入
    receive _ -> ok end
" 