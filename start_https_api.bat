@echo off
echo 启动HTTPS API服务器...

REM 检查Erlang是否安装
where erl >nul 2>nul
if %errorlevel% neq 0 (
    echo 错误: 未找到Erlang，请先安装Erlang/OTP
    pause
    exit /b 1
)

REM 检查OpenSSL是否安装
where openssl >nul 2>nul
if %errorlevel% neq 0 (
    echo 警告: 未找到OpenSSL，SSL证书生成可能失败
)

echo 正在启动HTTPS API服务器...
echo.

REM 启动Erlang shell并加载HTTPS API
erl -pa apps/*/ebin -eval "application:start(https_api), timer:sleep(2000), io:format('~n=== HTTPS API 服务器已启动 ===~n'), io:format('监听端口: 8443~n'), io:format('SDK登录接口: https://localhost:8443/api/v1/sdk/login~n'), io:format('后台管理接口: https://localhost:8443/api/v1/admin/stats~n'), io:format('~n按 Ctrl+C 退出~n'), receive _ -> ok end"

pause 