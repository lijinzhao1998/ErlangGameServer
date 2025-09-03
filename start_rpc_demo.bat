@echo off
echo 启动RPC系统演示...

REM 检查Erlang是否安装
where erl >nul 2>nul
if %errorlevel% neq 0 (
    echo 错误: 未找到Erlang，请先安装Erlang/OTP
    pause
    exit /b 1
)

echo 正在启动RPC系统演示...
echo.

REM 启动Erlang shell并加载RPC系统
erl -pa apps/*/ebin -eval "application:start(rpc), rpc_server:start_listener(8888), timer:sleep(2000), io:format('~n=== RPC系统已启动 ===~n'), io:format('监听端口: 8888~n'), io:format('服务注册表: 已启动~n'), io:format('监控系统: 已启动~n'), io:format('~n=== 演示命令 ===~n'), io:format('1. 运行测试: test_rpc:test_all().~n'), io:format('2. 运行演示: test_rpc:demo().~n'), io:format('3. 查看服务: rpc_api:list_services().~n'), io:format('4. 查看状态: rpc_api:get_stats().~n'), io:format('~n按 Ctrl+C 退出~n'), receive _ -> ok end"

pause 