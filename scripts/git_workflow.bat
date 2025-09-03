@echo off
chcp 65001 >nul
title Git工作流程管理

:MAIN_MENU
cls
echo ========================================
echo         Git工作流程管理
echo ========================================
echo.
echo 请选择工作流程:
echo.
echo [1] 开始新功能开发
echo [2] 完成功能开发
echo [3] 开始测试
echo [4] 完成测试
echo [5] 准备发布
echo [6] 发布完成
echo [7] 热修复
echo [8] 查看工作流程状态
echo [9] 查看分支关系图
echo [10] 清理已合并分支
echo [0] 退出
echo.
echo ========================================
echo.

set /p choice=请输入选择 (0-10): 

if "%choice%"=="0" goto EXIT
if "%choice%"=="1" goto START_FEATURE
if "%choice%"=="2" goto FINISH_FEATURE
if "%choice%"=="3" goto START_TESTING
if "%choice%"=="4" goto FINISH_TESTING
if "%choice%"=="5" goto PREPARE_RELEASE
if "%choice%"=="6" goto FINISH_RELEASE
if "%choice%"=="7" goto HOTFIX
if "%choice%"=="8" goto WORKFLOW_STATUS
if "%choice%"=="9" goto BRANCH_GRAPH
if "%choice%"=="10" goto CLEANUP_BRANCHES
echo 无效选择，请重新输入...
pause
goto MAIN_MENU

:START_FEATURE
cls
echo ========================================
echo          开始新功能开发
echo ========================================
echo.
echo 当前分支:
git branch
echo.
echo 确保在develop分支上开始新功能...
git checkout develop
echo.
echo 正在拉取最新更新...
git pull origin develop
echo.
set /p feature_name=请输入功能名称: 
if "%feature_name%"=="" (
    echo 功能名称不能为空！
    pause
    goto START_FEATURE
)
echo.
echo 正在创建功能分支...
git checkout -b feature/%feature_name%
echo.
echo 功能分支创建完成！
echo 当前分支: feature/%feature_name%
echo.
echo 现在可以开始开发功能了...
pause
goto MAIN_MENU

:FINISH_FEATURE
cls
echo ========================================
echo          完成功能开发
echo ========================================
echo.
echo 当前分支:
git branch
echo.
echo 请确保在功能分支上...
echo.
echo 检查工作区状态:
git status
echo.
set /p confirm=确认完成功能开发？(y/N): 
if /i "%confirm%"=="y" (
    echo 正在切换到develop分支...
    git checkout develop
    echo.
    echo 正在拉取最新更新...
    git pull origin develop
    echo.
    echo 正在合并功能分支...
    git merge --no-ff feature/%feature_name%
    echo.
    echo 正在删除功能分支...
    git branch -d feature/%feature_name%
    echo.
    echo 功能开发完成！已合并到develop分支。
) else (
    echo 操作已取消。
)
echo.
pause
goto MAIN_MENU

:START_TESTING
cls
echo ========================================
echo           开始测试
echo ========================================
echo.
echo 当前分支:
git branch
echo.
echo 确保在develop分支上...
git checkout develop
echo.
echo 正在拉取最新更新...
git pull origin develop
echo.
echo 正在创建测试分支...
git checkout -b testing/$(date /t)
echo.
echo 测试分支创建完成！
echo 当前分支: testing/$(date /t)
echo.
echo 现在可以开始测试了...
pause
goto MAIN_MENU

:FINISH_TESTING
cls
echo ========================================
echo           完成测试
echo ========================================
echo.
echo 当前分支:
git branch
echo.
echo 请确保在测试分支上...
echo.
set /p test_result=测试结果 (pass/fail): 
if /i "%test_result%"=="pass" (
    echo 测试通过！正在切换到develop分支...
    git checkout develop
    echo.
    echo 正在删除测试分支...
    git branch -D testing/$(date /t)
    echo.
    echo 测试完成！可以准备发布了。
) else (
    echo 测试失败！请修复问题后重新测试。
)
echo.
pause
goto MAIN_MENU

:PREPARE_RELEASE
cls
echo ========================================
echo           准备发布
echo ========================================
echo.
echo 当前分支:
git branch
echo.
echo 确保在develop分支上...
git checkout develop
echo.
echo 正在拉取最新更新...
git pull origin develop
echo.
set /p version=请输入版本号 (如: v1.0.0): 
if "%version%"=="" (
    echo 版本号不能为空！
    pause
    goto PREPARE_RELEASE
)
echo.
echo 正在创建发布分支...
git checkout -b release/%version%
echo.
echo 发布分支创建完成！
echo 当前分支: release/%version%
echo.
echo 现在可以进行最后的测试和修复...
pause
goto MAIN_MENU

:FINISH_RELEASE
cls
echo ========================================
echo           发布完成
echo ========================================
echo.
echo 当前分支:
git branch
echo.
echo 请确保在发布分支上...
echo.
set /p confirm=确认发布完成？(y/N): 
if /i "%confirm%"=="y" (
    echo 正在切换到main分支...
    git checkout main
    echo.
    echo 正在合并发布分支...
    git merge --no-ff release/%version%
    echo.
    echo 正在创建版本标签...
    git tag -a %version% -m "Release %version%"
    echo.
    echo 正在切换到develop分支...
    git checkout develop
    echo.
    echo 正在合并发布分支到develop...
    git merge --no-ff release/%version%
    echo.
    echo 正在删除发布分支...
    git branch -d release/%version%
    echo.
    echo 发布完成！版本 %version% 已发布。
) else (
    echo 操作已取消。
)
echo.
pause
goto MAIN_MENU

:HOTFIX
cls
echo ========================================
echo             热修复
echo ========================================
echo.
echo 当前分支:
git branch
echo.
echo 正在切换到main分支...
git checkout main
echo.
echo 正在拉取最新更新...
git pull origin main
echo.
set /p hotfix_name=请输入热修复名称: 
if "%hotfix_name%"=="" (
    echo 热修复名称不能为空！
    pause
    goto HOTFIX
)
echo.
echo 正在创建热修复分支...
git checkout -b hotfix/%hotfix_name%
echo.
echo 热修复分支创建完成！
echo 当前分支: hotfix/%hotfix_name%
echo.
echo 现在可以修复问题了...
echo 修复完成后，使用"完成发布"流程。
pause
goto MAIN_MENU

:WORKFLOW_STATUS
cls
echo ========================================
echo          工作流程状态
echo ========================================
echo.
echo 当前分支:
git branch
echo.
echo 本地分支状态:
git branch -v
echo.
echo 远程分支状态:
git branch -r
echo.
echo 最近提交:
git log --oneline -5
echo.
echo 工作区状态:
git status --short
echo.
pause
goto MAIN_MENU

:BRANCH_GRAPH
cls
echo ========================================
echo           分支关系图
echo ========================================
echo.
echo 分支关系图:
git log --graph --oneline --all --decorate -20
echo.
pause
goto MAIN_MENU

:CLEANUP_BRANCHES
cls
echo ========================================
echo          清理已合并分支
echo ========================================
echo.
echo 已合并到当前分支的分支:
git branch --merged
echo.
echo 未合并到当前分支的分支:
git branch --no-merged
echo.
set /p confirm=确认删除已合并的分支？(y/N): 
if /i "%confirm%"=="y" (
    echo 正在清理已合并的分支...
    git branch --merged | findstr -v "^\*\|main\|develop" | xargs -n 1 git branch -d
    echo 清理完成！
) else (
    echo 操作已取消。
)
echo.
pause
goto MAIN_MENU

:EXIT
exit 