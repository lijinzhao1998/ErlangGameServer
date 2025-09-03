@echo off
chcp 65001 >nul
title 服务器管理脚本

:menu
cls
echo ========================================
echo        服务器管理脚本
echo ========================================
echo.
echo 请选择要执行的操作:
echo.
echo 1. 重启服务器
echo 2. 热更新模块
echo 3. 清空数据库
echo 4. 查看服务器状态
echo 5. 停止所有服务器
echo 6. 编译项目
echo 7. 返回主菜单
echo 8. 退出
echo.
echo ========================================
set /p choice=请输入选择 (1-8): 

if "%choice%"=="1" goto restart_server
if "%choice%"=="2" goto hot_reload
if "%choice%"=="3" goto clear_database
if "%choice%"=="4" goto check_status
if "%choice%"=="5" goto stop_all
if "%choice%"=="6" goto compile_project
if "%choice%"=="7" goto main_menu
if "%choice%"=="8" goto exit
goto menu

:restart_server
cls
echo ========================================
echo           重启服务器
echo ========================================
echo.
echo 请选择要重启的服务器:
echo.
echo 1. 重启登录服务器
echo 2. 重启跨服服务器
echo 3. 重启游戏服务器
echo 4. 重启所有服务器
echo 5. 返回上级菜单
echo.
set /p restart_choice=请输入选择 (1-5): 

if "%restart_choice%"=="1" goto restart_login
if "%restart_choice%"=="2" goto restart_cross
if "%restart_choice%"=="3" goto restart_game
if "%restart_choice%"=="4" goto restart_all
if "%restart_choice%"=="5" goto menu
goto restart_server

:restart_login
echo 正在重启登录服务器...
taskkill /f /im erl.exe /fi "WINDOWTITLE eq 登录服务器*" >nul 2>&1
timeout /t 2 >nul
start "登录服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/login.config -s login_app start"
echo 登录服务器重启完成！
pause
goto menu

:restart_cross
echo 正在重启跨服服务器...
taskkill /f /im erl.exe /fi "WINDOWTITLE eq 跨服服务器*" >nul 2>&1
timeout /t 2 >nul
start "跨服服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/cross.config -s cross_app start"
echo 跨服服务器重启完成！
pause
goto menu

:restart_game
echo 正在重启游戏服务器...
taskkill /f /im erl.exe /fi "WINDOWTITLE eq 游戏服务器*" >nul 2>&1
timeout /t 2 >nul
start "游戏服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/game.config -s game_app start"
echo 游戏服务器重启完成！
pause
goto menu

:restart_all
echo 正在重启所有服务器...
taskkill /f /im erl.exe >nul 2>&1
timeout /t 3 >nul
start "登录服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/login.config -s login_app start"
timeout /t 2 >nul
start "跨服服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/cross.config -s cross_app start"
timeout /t 2 >nul
start "游戏服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/game.config -s game_app start"
echo 所有服务器重启完成！
pause
goto menu

:hot_reload
cls
echo ========================================
echo           热更新模块
echo ========================================
echo.
echo 请选择要热更新的模块:
echo.
echo 1. 热更新时间刷新管理器
echo 2. 热更新服务器配置
echo 3. 热更新时间工具
echo 4. 热更新所有模块
echo 5. 返回上级菜单
echo.
set /p hot_choice=请输入选择 (1-5): 

if "%hot_choice%"=="1" goto hot_reload_refresh
if "%hot_choice%"=="2" goto hot_reload_config
if "%hot_choice%"=="3" goto hot_reload_utils
if "%hot_choice%"=="4" goto hot_reload_all
if "%hot_choice%"=="5" goto menu
goto hot_reload

:hot_reload_refresh
echo 正在热更新时间刷新管理器...
cd /d %~dp0..
rebar3 compile
echo 编译完成，请在Erlang shell中执行: c:l(time_refresh_manager).
pause
goto menu

:hot_reload_config
echo 正在热更新服务器配置...
cd /d %~dp0..
rebar3 compile
echo 编译完成，请在Erlang shell中执行: c:l(server_config).
pause
goto menu

:hot_reload_utils
echo 正在热更新时间工具...
cd /d %~dp0..
rebar3 compile
echo 编译完成，请在Erlang shell中执行: c:l(time_utils).
pause
goto menu

:hot_reload_all
echo 正在热更新所有模块...
cd /d %~dp0..
rebar3 compile
echo 编译完成，请在Erlang shell中执行:
echo c:l(time_refresh_manager).
echo c:l(server_config).
echo c:l(time_utils).
pause
goto menu

:clear_database
cls
echo ========================================
echo           清空数据库
echo ========================================
echo.
echo 警告: 此操作将清空所有游戏数据！
echo.
echo 请选择要清空的数据库:
echo.
echo 1. 清空玩家数据
echo 2. 清空游戏数据
echo 3. 清空所有数据
echo 4. 返回上级菜单
echo.
set /p clear_choice=请输入选择 (1-4): 

if "%clear_choice%"=="1" goto clear_player_data
if "%clear_choice%"=="2" goto clear_game_data
if "%clear_choice%"=="3" goto clear_all_data
if "%clear_choice%"=="4" goto menu
goto clear_database

:clear_player_data
echo 正在清空玩家数据...
echo 请在Erlang shell中执行: mdb:clear_player_data().
pause
goto menu

:clear_game_data
echo 正在清空游戏数据...
echo 请在Erlang shell中执行: mdb:clear_game_data().
pause
goto menu

:clear_all_data
echo 正在清空所有数据...
echo 请在Erlang shell中执行: mdb:clear_all_data().
pause
goto menu

:check_status
cls
echo ========================================
echo           服务器状态
echo ========================================
echo.
echo 正在检查服务器状态...
echo.
tasklist /fi "imagename eq erl.exe" /fo table
echo.
echo 按任意键返回菜单...
pause >nul
goto menu

:stop_all
echo 正在停止所有服务器...
taskkill /f /im erl.exe >nul 2>&1
echo 所有服务器已停止！
pause
goto menu

:compile_project
echo 正在编译项目...
cd /d %~dp0..
rebar3 clean
rebar3 compile
if %errorlevel% equ 0 (
    echo 编译成功！
) else (
    echo 编译失败，请检查错误信息！
)
pause
goto menu

:main_menu
cd /d %~dp0
start_server.bat
exit

:exit
echo.
echo 感谢使用！
pause
exit 