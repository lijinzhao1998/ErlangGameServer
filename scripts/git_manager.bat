@echo off
chcp 65001 >nul
title Git管理工具

:MAIN_MENU
cls
echo ========================================
echo           Git管理工具
echo ========================================
echo.
echo 请选择操作:
echo.
echo [1] 查看Git状态
echo [2] 查看提交历史
echo [3] 添加所有文件到暂存区
echo [4] 提交更改
echo [5] 推送到远程仓库
echo [6] 从远程仓库拉取更新
echo [7] 查看分支信息
echo [8] 创建新分支
echo [9] 切换分支
echo [10] 合并分支
echo [11] 查看文件差异
echo [12] 撤销更改
echo [13] 查看远程仓库信息
echo [14] 初始化Git仓库
echo [15] 克隆远程仓库
echo [0] 返回主菜单
echo.
echo ========================================
echo.

set /p choice=请输入选择 (0-15): 

if "%choice%"=="0" goto EXIT
if "%choice%"=="1" goto GIT_STATUS
if "%choice%"=="2" goto GIT_LOG
if "%choice%"=="3" goto GIT_ADD
if "%choice%"=="4" goto GIT_COMMIT
if "%choice%"=="5" goto GIT_PUSH
if "%choice%"=="6" goto GIT_PULL
if "%choice%"=="7" goto GIT_BRANCH
if "%choice%"=="8" goto GIT_CREATE_BRANCH
if "%choice%"=="9" goto GIT_CHECKOUT
if "%choice%"=="10" goto GIT_MERGE
if "%choice%"=="11" goto GIT_DIFF
if "%choice%"=="12" goto GIT_RESET
if "%choice%"=="13" goto GIT_REMOTE
if "%choice%"=="14" goto GIT_INIT
if "%choice%"=="15" goto GIT_CLONE

echo 无效选择，请重新输入...
pause
goto MAIN_MENU

:GIT_STATUS
cls
echo ========================================
echo           查看Git状态
echo ========================================
echo.
git status
echo.
pause
goto MAIN_MENU

:GIT_LOG
cls
echo ========================================
echo           查看提交历史
echo ========================================
echo.
echo 显示最近的提交记录...
git log --oneline -10
echo.
pause
goto MAIN_MENU

:GIT_ADD
cls
echo ========================================
echo        添加所有文件到暂存区
echo ========================================
echo.
echo 正在添加所有文件...
git add .
echo.
echo 添加完成！当前状态：
git status
echo.
pause
goto MAIN_MENU

:GIT_COMMIT
cls
echo ========================================
echo             提交更改
echo ========================================
echo.
set /p commit_msg=请输入提交信息: 
if "%commit_msg%"=="" (
    echo 提交信息不能为空！
    pause
    goto GIT_COMMIT
)
echo.
echo 正在提交更改...
git commit -m "%commit_msg%"
echo.
echo 提交完成！
pause
goto MAIN_MENU

:GIT_PUSH
cls
echo ========================================
echo           推送到远程仓库
echo ========================================
echo.
echo 正在推送到远程仓库...
git push
echo.
echo 推送完成！
pause
goto MAIN_MENU

:GIT_PULL
cls
echo ========================================
echo         从远程仓库拉取更新
echo ========================================
echo.
echo 正在从远程仓库拉取更新...
git pull
echo.
echo 拉取完成！
pause
goto MAIN_MENU

:GIT_BRANCH
cls
echo ========================================
echo            查看分支信息
echo ========================================
echo.
echo 本地分支:
git branch
echo.
echo 远程分支:
git branch -r
echo.
echo 所有分支:
git branch -a
echo.
pause
goto MAIN_MENU

:GIT_CREATE_BRANCH
cls
echo ========================================
echo            创建新分支
echo ========================================
echo.
set /p branch_name=请输入新分支名称: 
if "%branch_name%"=="" (
    echo 分支名称不能为空！
    pause
    goto GIT_CREATE_BRANCH
)
echo.
echo 正在创建新分支...
git checkout -b %branch_name%
echo.
echo 分支创建完成！当前分支：
git branch
echo.
pause
goto MAIN_MENU

:GIT_CHECKOUT
cls
echo ========================================
echo             切换分支
echo ========================================
echo.
echo 当前分支:
git branch
echo.
set /p target_branch=请输入目标分支名称: 
if "%target_branch%"=="" (
    echo 分支名称不能为空！
    pause
    goto GIT_CHECKOUT
)
echo.
echo 正在切换分支...
git checkout %target_branch%
echo.
echo 分支切换完成！当前分支：
git branch
echo.
pause
goto MAIN_MENU

:GIT_MERGE
cls
echo ========================================
echo             合并分支
echo ========================================
echo.
echo 当前分支:
git branch
echo.
set /p merge_branch=请输入要合并的分支名称: 
if "%merge_branch%"=="" (
    echo 分支名称不能为空！
    pause
    goto GIT_MERGE
)
echo.
echo 正在合并分支...
git merge %merge_branch%
echo.
echo 合并完成！
pause
goto MAIN_MENU

:GIT_DIFF
cls
echo ========================================
echo            查看文件差异
echo ========================================
echo.
echo 显示工作区与暂存区的差异...
git diff
echo.
pause
goto MAIN_MENU

:GIT_RESET
cls
echo ========================================
echo             撤销更改
echo ========================================
echo.
echo 请选择撤销操作:
echo [1] 撤销工作区的更改
echo [2] 撤销暂存区的更改
echo [3] 撤销最后一次提交
echo [4] 返回主菜单
echo.
set /p reset_choice=请输入选择 (1-4): 

if "%reset_choice%"=="1" (
    echo 正在撤销工作区的更改...
    git checkout -- .
    echo 撤销完成！
) else if "%reset_choice%"=="2" (
    echo 正在撤销暂存区的更改...
    git reset HEAD
    echo 撤销完成！
) else if "%reset_choice%"=="3" (
    echo 正在撤销最后一次提交...
    git reset --soft HEAD~1
    echo 撤销完成！
) else if "%reset_choice%"=="4" (
    goto MAIN_MENU
) else (
    echo 无效选择！
)
echo.
pause
goto MAIN_MENU

:GIT_REMOTE
cls
echo ========================================
echo          查看远程仓库信息
echo ========================================
echo.
echo 远程仓库列表:
git remote -v
echo.
pause
goto MAIN_MENU

:GIT_INIT
cls
echo ========================================
echo           初始化Git仓库
echo ========================================
echo.
echo 警告：这将初始化一个新的Git仓库！
echo 如果当前目录已经是Git仓库，此操作将被忽略。
echo.
set /p confirm=确认继续？(y/N): 
if /i "%confirm%"=="y" (
    echo 正在初始化Git仓库...
    git init
    echo.
    echo 初始化完成！
) else (
    echo 操作已取消。
)
echo.
pause
goto MAIN_MENU

:GIT_CLONE
cls
echo ========================================
echo           克隆远程仓库
echo ========================================
echo.
echo 警告：这将克隆一个新的仓库到当前目录！
echo 请确保当前目录为空。
echo.
set /p repo_url=请输入远程仓库URL: 
if "%repo_url%"=="" (
    echo 仓库URL不能为空！
    pause
    goto GIT_CLONE
)
echo.
echo 正在克隆仓库...
git clone %repo_url%
echo.
echo 克隆完成！
pause
goto MAIN_MENU

:EXIT
exit 