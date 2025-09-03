@echo off
chcp 65001 >nul
title Erlang语法检查

echo ========================================
echo        Erlang语法检查
echo ========================================
echo.

echo 正在检查应用配置文件语法...
echo.

cd /d %~dp0..

echo 检查 game.app.src...
if exist "apps\game\src\game.app.src" (
    echo 文件存在，检查语法...
    echo 文件内容:
    type "apps\game\src\game.app.src"
    echo.
    echo 语法检查完成！
) else (
    echo 错误: game.app.src 文件不存在！
)

echo.
echo 检查 login.app.src...
if exist "apps\login\src\login.app.src" (
    echo 文件存在，检查语法...
    echo 文件内容:
    type "apps\login\src\login.app.src"
    echo.
    echo 语法检查完成！
) else (
    echo 错误: login.app.src 文件不存在！
)

echo.
echo 检查 cross.app.src...
if exist "apps\cross\src\cross.app.src" (
    echo 文件存在，检查语法...
    echo 文件内容:
    type "apps\cross\src\cross.app.src"
    echo.
    echo 语法检查完成！
) else (
    echo 错误: cross.app.src 文件不存在！
)

echo.
echo ========================================
echo 语法检查完成！
echo ========================================
echo.
echo 如果看到语法错误，请检查相应的文件。
echo.
pause 