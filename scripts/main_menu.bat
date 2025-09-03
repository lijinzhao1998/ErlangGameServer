@echo off
chcp 65001 >nul
title Erlang游戏服务器主菜单

:menu
cls
echo ========================================
echo        Erlang游戏服务器主菜单
echo ========================================
echo.
echo 请选择要执行的操作:
echo.
echo 1. 启动服务器
echo 2. 服务器管理
echo 3. 热更新
echo 4. 数据库管理
echo 5. 一键启动所有服务器
echo 6. 编译项目
echo 7. Git管理工具
echo 8. Git快速操作
echo 9. Git工作流程
echo 10. 退出
echo.
echo ========================================
set /p choice=请输入选择 (1-10): 

if "%choice%"=="1" goto start_server
if "%choice%"=="2" goto server_management
if "%choice%"=="3" goto hot_reload
if "%choice%"=="4" goto database_management
if "%choice%"=="5" goto quick_start_all
if "%choice%"=="6" goto compile_project
if "%choice%"=="7" goto git_management
if "%choice%"=="8" goto git_quick
if "%choice%"=="9" goto git_workflow
if "%choice%"=="10" goto exit
goto menu

:start_server
cls
echo 正在启动服务器管理界面...
cd /d %~dp0
start_server.bat
exit

:server_management
cls
echo 正在启动服务器管理界面...
cd /d %~dp0
server_manager.bat
exit

:hot_reload
cls
echo 正在启动热更新界面...
cd /d %~dp0
hot_reload.bat
exit

:database_management
cls
echo 正在启动数据库管理界面...
cd /d %~dp0
database_manager.bat
exit

:quick_start_all
cls
echo 正在启动一键启动脚本...
cd /d %~dp0
quick_start.bat
exit

:compile_project
cls
echo ========================================
echo           编译项目
echo ========================================
echo.
echo 正在编译项目...
cd /d %~dp0..
echo 当前目录: %cd%
echo.
echo 执行清理...
rebar3 clean
echo.
echo 执行编译...
rebar3 compile

if %errorlevel% equ 0 (
    echo.
    echo ========================================
    echo 编译成功！
    echo ========================================
    echo.
    echo 项目已成功编译，可以启动服务器了。
) else (
    echo.
    echo ========================================
    echo 编译失败！
    echo.
    echo 请检查错误信息并修复问题。
)

echo.
echo 按任意键返回主菜单...
pause >nul
goto menu

:git_management
cls
echo 正在启动Git管理工具...
cd /d %~dp0
git_manager.bat
exit

:git_quick
cls
echo 正在启动Git快速操作...
cd /d %~dp0
git_quick.bat
exit

:git_workflow
cls
echo 正在启动Git工作流程管理...
cd /d %~dp0
git_workflow.bat
exit

:exit
cls
echo ========================================
echo           感谢使用
echo ========================================
echo.
echo 感谢使用Erlang游戏服务器管理脚本！
echo.
echo 如有问题，请查看项目文档或联系开发团队。
echo.
echo 按任意键退出...
pause >nul
exit 