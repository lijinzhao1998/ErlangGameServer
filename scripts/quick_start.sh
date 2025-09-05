#!/bin/bash

# 设置UTF-8编码
export LANG=zh_CN.UTF-8

# 获取脚本所在目录的父目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

clear
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}        一键启动所有服务器${NC}"
echo -e "${BLUE}========================================${NC}"
echo
echo -e "${YELLOW}正在启动所有服务器...${NC}"
echo

cd "$PROJECT_ROOT"

echo -e "${YELLOW}1. 启动登录服务器...${NC}"
gnome-terminal --title="登录服务器" -- bash -c "rebar3 shell --config config/login.config -s login_app start; exec bash" 2>/dev/null || \
xterm -title "登录服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/login.config -s login_app start; exec bash" 2>/dev/null || \
(echo "无法打开新终端，在当前终端启动登录服务器..." && rebar3 shell --config config/login.config -s login_app start)
sleep 3

echo -e "${YELLOW}2. 启动跨服服务器...${NC}"
gnome-terminal --title="跨服服务器" -- bash -c "rebar3 shell --config config/cross.config -s cross_app start; exec bash" 2>/dev/null || \
xterm -title "跨服服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/cross.config -s cross_app start; exec bash" 2>/dev/null || \
(echo "无法打开新终端，在当前终端启动跨服服务器..." && rebar3 shell --config config/cross.config -s cross_app start)
sleep 3

echo -e "${YELLOW}3. 启动游戏服务器...${NC}"
gnome-terminal --title="游戏服务器" -- bash -c "rebar3 shell --config config/game.config -s game_app start; exec bash" 2>/dev/null || \
xterm -title "游戏服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/game.config -s game_app start; exec bash" 2>/dev/null || \
(echo "无法打开新终端，在当前终端启动游戏服务器..." && rebar3 shell --config config/game.config -s game_app start)

echo
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}所有服务器启动完成！${NC}"
echo
echo -e "${YELLOW}服务器窗口已打开，请检查启动状态。${NC}"
echo -e "${YELLOW}如需管理服务器，请运行 ./scripts/server_manager.sh${NC}"
echo
echo "按任意键退出..."
read -n 1 -s