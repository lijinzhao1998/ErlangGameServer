@echo off
chcp 65001 >nul
title Erlang游戏服务器

echo ========================================
echo        Erlang游戏服务器
echo ========================================
echo.
echo 正在启动管理界面...
echo.

cd /d %~dp0
main_menu.bat 