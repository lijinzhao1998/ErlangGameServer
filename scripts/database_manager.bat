@echo off
chcp 65001 >nul
title 数据库管理脚本

:menu
cls
echo ========================================
echo           数据库管理脚本
echo ========================================
echo.
echo 请选择要执行的操作:
echo.
echo 1. 清空数据库
echo 2. 备份数据库
echo 3. 恢复数据库
echo 4. 查看数据库状态
echo 5. 重置数据库
echo 6. 返回上级菜单
echo 7. 退出
echo.
echo ========================================
set /p choice=请输入选择 (1-7): 

if "%choice%"=="1" goto clear_database
if "%choice%"=="2" goto backup_database
if "%choice%"=="3" goto restore_database
if "%choice%"=="4" goto check_database_status
if "%choice%"=="5" goto reset_database
if "%choice%"=="6" goto main_menu
if "%choice%"=="7" goto exit
goto menu

:clear_database
cls
echo ========================================
echo           清空数据库
echo ========================================
echo.
echo 警告: 此操作将清空所有游戏数据！
echo 请确认您要执行此操作。
echo.
echo 请选择要清空的数据库类型:
echo.
echo 1. 清空玩家数据 (角色、装备、背包等)
echo 2. 清空游戏数据 (任务、活动、排行榜等)
echo 3. 清空系统数据 (配置、日志等)
echo 4. 清空所有数据
echo 5. 返回上级菜单
echo.
set /p clear_choice=请输入选择 (1-5): 

if "%clear_choice%"=="1" goto clear_player_data
if "%clear_choice%"=="2" goto clear_game_data
if "%clear_choice%"=="3" goto clear_system_data
if "%clear_choice%"=="4" goto clear_all_data
if "%clear_choice%"=="5" goto menu
goto clear_database

:clear_player_data
cls
echo ========================================
echo           清空玩家数据
echo ========================================
echo.
echo 即将清空以下玩家数据:
echo - 角色信息
echo - 装备数据
echo - 背包物品
echo - 技能数据
echo - 成就数据
echo.
echo 警告: 此操作不可恢复！
echo.
set /p confirm=确认清空玩家数据? (y/N): 
if /i "%confirm%"=="y" (
    echo.
    echo 正在清空玩家数据...
    echo 请在Erlang shell中执行以下命令:
    echo.
    echo %% 清空玩家数据
    echo mdb:clear_player_data().
    echo.
    echo %% 或者清空特定表
    echo mdb:clear_table("players").
    echo mdb:clear_table("equipment").
    echo mdb:clear_table("inventory").
    echo mdb:clear_table("skills").
    echo mdb:clear_table("achievements").
    echo.
    echo 清空完成！
) else (
    echo 操作已取消。
)
pause
goto menu

:clear_game_data
cls
echo ========================================
echo           清空游戏数据
echo ========================================
echo.
echo 即将清空以下游戏数据:
echo - 任务数据
echo - 活动数据
echo - 排行榜数据
echo - Boss数据
echo - 副本数据
echo.
echo 警告: 此操作不可恢复！
echo.
set /p confirm=确认清空游戏数据? (y/N): 
if /i "%confirm%"=="y" (
    echo.
    echo 正在清空游戏数据...
    echo 请在Erlang shell中执行以下命令:
    echo.
    echo %% 清空游戏数据
    echo mdb:clear_game_data().
    echo.
    echo %% 或者清空特定表
    echo mdb:clear_table("quests").
    echo mdb:clear_table("activities").
    echo mdb:clear_table("rankings").
    echo mdb:clear_table("bosses").
    echo mdb:clear_table("dungeons").
    echo.
    echo 清空完成！
) else (
    echo 操作已取消。
)
pause
goto menu

:clear_system_data
cls
echo ========================================
echo           清空系统数据
echo ========================================
echo.
echo 即将清空以下系统数据:
echo - 服务器配置
echo - 系统日志
echo - 统计信息
echo - 缓存数据
echo.
echo 警告: 此操作不可恢复！
echo.
set /p confirm=确认清空系统数据? (y/N): 
if /i "%confirm%"=="y" (
    echo.
    echo 正在清空系统数据...
    echo 请在Erlang shell中执行以下命令:
    echo.
    echo %% 清空系统数据
    echo mdb:clear_system_data().
    echo.
    echo %% 或者清空特定表
    echo mdb:clear_table("server_config").
    echo mdb:clear_table("system_logs").
    echo mdb:clear_table("statistics").
    echo mdb:clear_table("cache").
    echo.
    echo 清空完成！
) else (
    echo 操作已取消。
)
pause
goto menu

:clear_all_data
cls
echo ========================================
echo           清空所有数据
echo ========================================
echo.
echo 即将清空所有数据库数据！
echo.
echo 警告: 此操作不可恢复！
echo 所有游戏数据、玩家数据、系统数据都将被清空！
echo.
set /p confirm=确认清空所有数据? (y/N): 
if /i "%confirm%"=="y" (
    echo.
    echo 正在清空所有数据...
    echo 请在Erlang shell中执行以下命令:
    echo.
    echo %% 清空所有数据
    echo mdb:clear_all_data().
    echo.
    echo %% 或者清空所有表
    echo mdb:clear_all_tables().
    echo.
    echo %% 重置数据库
    echo mdb:reset_database().
    echo.
    echo 清空完成！
) else (
    echo 操作已取消。
)
pause
goto menu

:backup_database
cls
echo ========================================
echo           备份数据库
echo ========================================
echo.
echo 请选择备份方式:
echo.
echo 1. 备份所有数据
echo 2. 备份玩家数据
echo 3. 备份游戏数据
echo 4. 备份系统数据
echo 5. 返回上级菜单
echo.
set /p backup_choice=请输入选择 (1-5): 

if "%backup_choice%"=="1" goto backup_all_data
if "%backup_choice%"=="2" goto backup_player_data
if "%backup_choice%"=="3" goto backup_game_data
if "%backup_choice%"=="4" goto backup_system_data
if "%backup_choice%"=="5" goto menu
goto backup_database

:backup_all_data
echo.
echo 正在备份所有数据...
echo 请在Erlang shell中执行以下命令:
echo.
echo %% 备份所有数据
echo mdb:backup_all_data("backup_all_" ++ date() ++ ".dat").
echo.
echo %% 或者使用时间戳命名
echo mdb:backup_all_data("backup_" ++ integer_to_list(erlang:system_time(second)) ++ ".dat").
pause
goto menu

:backup_player_data
echo.
echo 正在备份玩家数据...
echo 请在Erlang shell中执行以下命令:
echo.
echo %% 备份玩家数据
echo mdb:backup_player_data("backup_players_" ++ date() ++ ".dat").
pause
goto menu

:backup_game_data
echo.
echo 正在备份游戏数据...
echo 请在Erlang shell中执行以下命令:
echo.
echo %% 备份游戏数据
echo mdb:backup_game_data("backup_game_" ++ date() ++ ".dat").
pause
goto menu

:backup_system_data
echo.
echo 正在备份系统数据...
echo 请在Erlang shell中执行以下命令:
echo.
echo %% 备份系统数据
echo mdb:backup_system_data("backup_system_" ++ date() ++ ".dat").
pause
goto menu

:restore_database
cls
echo ========================================
echo           恢复数据库
echo ========================================
echo.
echo 请选择恢复方式:
echo.
echo 1. 从备份文件恢复
echo 2. 从指定文件恢复
echo 3. 返回上级菜单
echo.
set /p restore_choice=请输入选择 (1-3): 

if "%restore_choice%"=="1" goto restore_from_backup
if "%restore_choice%"=="2" goto restore_from_file
if "%restore_choice%"=="3" goto menu
goto restore_database

:restore_from_backup
echo.
echo 正在从备份文件恢复...
echo 请在Erlang shell中执行以下命令:
echo.
echo %% 列出可用备份文件
echo mdb:list_backup_files().
echo.
echo %% 从备份文件恢复
echo mdb:restore_from_backup("backup_filename.dat").
pause
goto menu

:restore_from_file
echo.
echo 请输入要恢复的文件路径:
set /p file_path=文件路径: 
if "%file_path%"=="" goto restore_from_file

echo.
echo 正在从文件 %file_path% 恢复...
echo 请在Erlang shell中执行以下命令:
echo.
echo %% 从指定文件恢复
echo mdb:restore_from_file("%file_path%").
pause
goto menu

:check_database_status
cls
echo ========================================
echo           数据库状态
echo ========================================
echo.
echo 正在检查数据库状态...
echo 请在Erlang shell中执行以下命令:
echo.
echo %% 检查数据库连接状态
echo mdb:status().
echo.
echo %% 检查数据库大小
echo mdb:get_database_size().
echo.
echo %% 检查表数量
echo mdb:get_table_count().
echo.
echo %% 列出所有表
echo mdb:list_tables().
echo.
echo %% 检查表状态
echo mdb:check_table_status("table_name").
pause
goto menu

:reset_database
cls
echo ========================================
echo           重置数据库
echo ========================================
echo.
echo 即将重置整个数据库！
echo.
echo 警告: 此操作不可恢复！
echo 所有数据将被清空，数据库将恢复到初始状态！
echo.
set /p confirm=确认重置数据库? (y/N): 
if /i "%confirm%"=="y" (
    echo.
    echo 正在重置数据库...
    echo 请在Erlang shell中执行以下命令:
    echo.
    echo %% 重置数据库
    echo mdb:reset_database().
    echo.
    echo %% 或者重新初始化
    echo mdb:reinitialize().
    echo.
    echo 重置完成！
) else (
    echo 操作已取消。
)
pause
goto menu

:main_menu
cd /d %~dp0
server_manager.bat
exit

:exit
echo.
echo 感谢使用数据库管理脚本！
pause
exit 