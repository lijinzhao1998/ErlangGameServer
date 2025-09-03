@echo off
chcp 65001 >nul
title Erlang游戏服务器管理脚本

:menu
cls
echo ========================================
echo        Erlang游戏服务器管理脚本
echo ========================================
echo.
echo 请选择要启动的服务器类型:
echo.
echo 1. 启动登录服务器
echo 2. 启动跨服服务器  
echo 3. 启动游戏服务器
echo 4. 启动所有服务器
echo 5. 退出
echo.
echo ========================================
set /p choice=请输入选择 (1-5): 

if "%choice%"=="1" goto start_login
if "%choice%"=="2" goto start_cross
if "%choice%"=="3" goto start_game
if "%choice%"=="4" goto start_all
if "%choice%"=="5" goto exit
goto menu

:start_login
echo.
echo 正在启动登录服务器...
start "登录服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/login.config -s login_app start"
echo 登录服务器启动完成！
timeout /t 3 >nul
goto menu

:start_cross
echo.
echo 正在启动跨服服务器...
start "跨服服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/cross.config -s cross_app start"
echo 跨服服务器启动完成！
timeout /t 3 >nul
goto menu

:start_game
echo.
echo 正在启动游戏服务器...
start "游戏服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/game.config -s game_app start"
echo 游戏服务器启动完成！
timeout /t 3 >nul
goto menu

:start_all
echo.
echo 正在启动所有服务器...
start "登录服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/login.config -s login_app start"
timeout /t 2 >nul
start "跨服服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/cross.config -s cross_app start"
timeout /t 2 >nul
start "游戏服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/game.config -s game_app start"
echo 所有服务器启动完成！
timeout /t 3 >nul
goto menu

:exit
echo.
echo 感谢使用！
pause
exit 