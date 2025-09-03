@echo off
chcp 65001 >nul
title 热更新脚本

:menu
cls
echo ========================================
echo           热更新脚本
echo ========================================
echo.
echo 请选择热更新方式:
echo.
echo 1. 编译并热更新指定模块
echo 2. 编译并热更新所有模块
echo 3. 热更新时间相关模块
echo 4. 热更新数据库相关模块
echo 5. 热更新网络相关模块
echo 6. 返回上级菜单
echo 7. 退出
echo.
echo ========================================
set /p choice=请输入选择 (1-7): 

if "%choice%"=="1" goto hot_reload_specific
if "%choice%"=="2" goto hot_reload_all
if "%choice%"=="3" goto hot_reload_time
if "%choice%"=="4" goto hot_reload_db
if "%choice%"=="5" goto hot_reload_network
if "%choice%"=="6" goto main_menu
if "%choice%"=="7" goto exit
goto menu

:hot_reload_specific
cls
echo ========================================
echo        编译并热更新指定模块
echo ========================================
echo.
echo 请输入要热更新的模块名 (不含.erl扩展名):
set /p module_name=模块名: 

if "%module_name%"=="" goto hot_reload_specific

echo.
echo 正在编译模块 %module_name%...
cd /d %~dp0..
rebar3 compile

if %errorlevel% equ 0 (
    echo.
    echo 编译成功！
    echo.
    echo 请在Erlang shell中执行以下命令进行热更新:
    echo c:l(%module_name%).
    echo.
    echo 或者使用code模块:
    echo code:load_file(%module_name%).
) else (
    echo.
    echo 编译失败，请检查错误信息！
)

pause
goto menu

:hot_reload_all
cls
echo ========================================
echo        编译并热更新所有模块
echo ========================================
echo.
echo 正在编译所有模块...
cd /d %~dp0..
rebar3 clean
rebar3 compile

if %errorlevel% equ 0 (
    echo.
    echo 编译成功！
    echo.
    echo 请在Erlang shell中执行以下命令进行热更新:
    echo.
    echo %% 时间相关模块
    echo c:l(time_refresh_manager).
    echo c:l(time_utils).
    echo.
    echo %% 配置相关模块
    echo c:l(server_config).
    echo.
    echo %% 刷新相关模块
    echo c:l(refresh_examples).
    echo.
    echo %% 其他模块
    echo c:l(cross_sup).
    echo c:l(cross_app).
) else (
    echo.
    echo 编译失败，请检查错误信息！
)

pause
goto menu

:hot_reload_time
cls
echo ========================================
echo        热更新时间相关模块
echo ========================================
echo.
echo 正在编译时间相关模块...
cd /d %~dp0..
rebar3 compile

if %errorlevel% equ 0 (
    echo.
    echo 编译成功！
    echo.
    echo 请在Erlang shell中执行以下命令:
    echo.
    echo %% 热更新时间刷新管理器
    echo c:l(time_refresh_manager).
    echo.
    echo %% 热更新时间工具
    echo c:l(time_utils).
    echo.
    echo %% 热更新刷新示例
    echo c:l(refresh_examples).
    echo.
    echo %% 重新注册刷新处理器
    echo refresh_examples:register_all_refresh().
) else (
    echo.
    echo 编译失败，请检查错误信息！
)

pause
goto menu

:hot_reload_db
cls
echo ========================================
echo        热更新数据库相关模块
echo ========================================
echo.
echo 正在编译数据库相关模块...
cd /d %~dp0..
rebar3 compile

if %errorlevel% equ 0 (
    echo.
    echo 编译成功！
    echo.
    echo 请在Erlang shell中执行以下命令:
    echo.
    echo %% 热更新MDB数据库模块
    echo c:l(mdb).
    echo.
    echo %% 热更新玩家认证模块
    echo c:l(player_auth).
    echo.
    echo %% 检查数据库连接状态
    echo mdb:status().
) else (
    echo.
    echo 编译失败，请检查错误信息！
)

pause
goto menu

:hot_reload_network
cls
echo ========================================
echo        热更新网络相关模块
echo ========================================
echo.
echo 正在编译网络相关模块...
cd /d %~dp0..
rebar3 compile

if %errorlevel% equ 0 (
    echo.
    echo 编译成功！
    echo.
    echo 请在Erlang shell中执行以下命令:
    echo.
    echo %% 热更新跨服监听器
    echo c:l(cross_listener).
    echo.
    echo %% 热更新动态连接监督者
    echo c:l(dynamic_conn_sup).
    echo.
    echo %% 检查网络状态
    echo cross_listener:status().
) else (
    echo.
    echo 编译失败，请检查错误信息！
)

pause
goto menu

:main_menu
cd /d %~dp0
server_manager.bat
exit

:exit
echo.
echo 感谢使用热更新脚本！
pause
exit 