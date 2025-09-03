@echo off
chcp 65001 >nul
title Git快速操作

:MAIN_MENU
cls
echo ========================================
echo          Git快速操作
echo ========================================
echo.
echo 请选择操作:
echo.
echo [1] 快速提交 (add + commit)
echo [2] 快速推送 (add + commit + push)
echo [3] 快速拉取并合并
echo [4] 查看当前状态
echo [5] 查看最近提交
echo [6] 快速创建功能分支
echo [7] 快速合并并删除分支
echo [8] 快速撤销最后提交
echo [9] 快速清理工作区
echo [0] 退出
echo.
echo ========================================
echo.

set /p choice=请输入选择 (0-9): 

if "%choice%"=="0" goto EXIT
if "%choice%"=="1" goto QUICK_COMMIT
if "%choice%"=="2" goto QUICK_PUSH
if "%choice%"=="3" goto QUICK_PULL
if "%choice%"=="4" goto QUICK_STATUS
if "%choice%"=="5" goto QUICK_LOG
if "%choice%"=="6" goto QUICK_FEATURE_BRANCH
if "%choice%"=="7" goto QUICK_MERGE_DELETE
if "%choice%"=="8" goto QUICK_UNDO_COMMIT
if "%choice%"=="9" goto QUICK_CLEAN
echo 无效选择，请重新输入...
pause
goto MAIN_MENU

:QUICK_COMMIT
cls
echo ========================================
echo           快速提交
echo ========================================
echo.
echo 当前状态:
git status --short
echo.
set /p commit_msg=请输入提交信息: 
if "%commit_msg%"=="" (
    echo 提交信息不能为空！
    pause
    goto QUICK_COMMIT
)
echo.
echo 正在快速提交...
git add .
git commit -m "%commit_msg%"
echo.
echo 提交完成！
pause
goto MAIN_MENU

:QUICK_PUSH
cls
echo ========================================
echo           快速推送
echo ========================================
echo.
echo 当前状态:
git status --short
echo.
set /p commit_msg=请输入提交信息: 
if "%commit_msg%"=="" (
    echo 提交信息不能为空！
    pause
    goto QUICK_PUSH
)
echo.
echo 正在快速推送...
git add .
git commit -m "%commit_msg%"
git push
echo.
echo 推送完成！
pause
goto MAIN_MENU

:QUICK_PULL
cls
echo ========================================
echo         快速拉取并合并
echo ========================================
echo.
echo 正在拉取远程更新...
git fetch
echo.
echo 远程分支状态:
git branch -r
echo.
echo 正在合并远程更新...
git pull
echo.
echo 拉取完成！
pause
goto MAIN_MENU

:QUICK_STATUS
cls
echo ========================================
echo           当前状态
echo ========================================
echo.
echo 工作区状态:
git status --short
echo.
echo 分支信息:
git branch -v
echo.
echo 最近提交:
git log --oneline -3
echo.
pause
goto MAIN_MENU

:QUICK_LOG
cls
echo ========================================
echo           最近提交
echo ========================================
echo.
echo 最近10次提交:
git log --oneline -10
echo.
echo 最近5次详细提交:
git log --oneline -5 --stat
echo.
pause
goto MAIN_MENU

:QUICK_FEATURE_BRANCH
cls
echo ========================================
echo         快速创建功能分支
echo ========================================
echo.
set /p feature_name=请输入功能名称: 
if "%feature_name%"=="" (
    echo 功能名称不能为空！
    pause
    goto QUICK_FEATURE_BRANCH
)
echo.
echo 正在创建功能分支...
git checkout -b feature/%feature_name%
echo.
echo 功能分支创建完成！
echo 当前分支:
git branch
echo.
pause
goto MAIN_MENU

:QUICK_MERGE_DELETE
cls
echo ========================================
echo       快速合并并删除分支
echo ========================================
echo.
echo 当前分支:
git branch
echo.
set /p merge_branch=请输入要合并的分支名称: 
if "%merge_branch%"=="" (
    echo 分支名称不能为空！
    pause
    goto QUICK_MERGE_DELETE
)
echo.
echo 正在合并分支...
git merge %merge_branch%
echo.
echo 正在删除已合并的分支...
git branch -d %merge_branch%
echo.
echo 合并和删除完成！
pause
goto MAIN_MENU

:QUICK_UNDO_COMMIT
cls
echo ========================================
echo         快速撤销最后提交
echo ========================================
echo.
echo 最后3次提交:
git log --oneline -3
echo.
set /p confirm=确认撤销最后一次提交？(y/N): 
if /i "%confirm%"=="y" (
    echo 正在撤销最后提交...
    git reset --soft HEAD~1
    echo 撤销完成！文件已回到暂存区。
) else (
    echo 操作已取消。
)
echo.
pause
goto MAIN_MENU

:QUICK_CLEAN
cls
echo ========================================
echo           快速清理工作区
echo ========================================
echo.
echo 当前状态:
git status
echo.
echo 未跟踪的文件:
git status --porcelain | findstr "^??"
echo.
set /p confirm=确认清理未跟踪的文件？(y/N): 
if /i "%confirm%"=="y" (
    echo 正在清理未跟踪的文件...
    git clean -fd
    echo 清理完成！
) else (
    echo 操作已取消。
)
echo.
pause
goto MAIN_MENU

:EXIT
exit 