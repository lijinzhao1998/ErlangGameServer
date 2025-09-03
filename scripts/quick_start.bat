@echo off
chcp 65001 >nul
title 一键启动所有服务器

echo ========================================
echo        一键启动所有服务器
echo ========================================
echo.
echo 正在启动所有服务器...
echo.

echo 1. 启动登录服务器...
start "登录服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/login.config -s login_app start"
timeout /t 3 >nul

echo 2. 启动跨服服务器...
start "跨服服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/cross.config -s cross_app start"
timeout /t 3 >nul

echo 3. 启动游戏服务器...
start "游戏服务器" cmd /k "cd /d %~dp0.. && rebar3 shell --config config/game.config -s game_app start"
timeout /t 3 >nul

echo.
echo ========================================
echo 所有服务器启动完成！
echo.
echo 服务器窗口已打开，请检查启动状态。
echo 如需管理服务器，请运行 server_manager.bat
echo.
echo 按任意键退出...
pause >nul 