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

# 显示菜单
show_menu() {
    clear
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}        Erlang游戏服务器管理脚本${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo
    echo "请选择要启动的服务器类型:"
    echo
    echo "1. 启动登录服务器"
    echo "2. 启动跨服服务器"
    echo "3. 启动游戏服务器"
    echo "4. 启动所有服务器"
    echo "5. 退出"
    echo
    echo -e "${BLUE}========================================${NC}"
}

# 启动登录服务器
start_login() {
    echo
    echo -e "${YELLOW}正在启动登录服务器...${NC}"
    cd "$PROJECT_ROOT"
    gnome-terminal --title="登录服务器" -- bash -c "rebar3 shell --config config/login.config -s login_app start; exec bash" 2>/dev/null || \
    xterm -title "登录服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/login.config -s login_app start; exec bash" 2>/dev/null || \
    (echo "无法打开新终端，在当前终端启动..." && rebar3 shell --config config/login.config -s login_app start)
    echo -e "${GREEN}登录服务器启动完成！${NC}"
    sleep 3
}

# 启动跨服服务器
start_cross() {
    echo
    echo -e "${YELLOW}正在启动跨服服务器...${NC}"
    cd "$PROJECT_ROOT"
    gnome-terminal --title="跨服服务器" -- bash -c "rebar3 shell --config config/cross.config -s cross_app start; exec bash" 2>/dev/null || \
    xterm -title "跨服服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/cross.config -s cross_app start; exec bash" 2>/dev/null || \
    (echo "无法打开新终端，在当前终端启动..." && rebar3 shell --config config/cross.config -s cross_app start)
    echo -e "${GREEN}跨服服务器启动完成！${NC}"
    sleep 3
}

# 启动游戏服务器
start_game() {
    echo
    echo -e "${YELLOW}正在启动游戏服务器...${NC}"
    cd "$PROJECT_ROOT"
    gnome-terminal --title="游戏服务器" -- bash -c "rebar3 shell --config config/game.config -s game_app start; exec bash" 2>/dev/null || \
    xterm -title "游戏服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/game.config -s game_app start; exec bash" 2>/dev/null || \
    (echo "无法打开新终端，在当前终端启动..." && rebar3 shell --config config/game.config -s game_app start)
    echo -e "${GREEN}游戏服务器启动完成！${NC}"
    sleep 3
}

# 启动所有服务器
start_all() {
    echo
    echo -e "${YELLOW}正在启动所有服务器...${NC}"
    cd "$PROJECT_ROOT"
    
    # 启动登录服务器
    gnome-terminal --title="登录服务器" -- bash -c "rebar3 shell --config config/login.config -s login_app start; exec bash" 2>/dev/null || \
    xterm -title "登录服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/login.config -s login_app start; exec bash" 2>/dev/null || \
    (echo "无法打开新终端，在当前终端启动登录服务器..." && rebar3 shell --config config/login.config -s login_app start)
    sleep 2
    
    # 启动跨服服务器
    gnome-terminal --title="跨服服务器" -- bash -c "rebar3 shell --config config/cross.config -s cross_app start; exec bash" 2>/dev/null || \
    xterm -title "跨服服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/cross.config -s cross_app start; exec bash" 2>/dev/null || \
    (echo "无法打开新终端，在当前终端启动跨服服务器..." && rebar3 shell --config config/cross.config -s cross_app start)
    sleep 2
    
    # 启动游戏服务器
    gnome-terminal --title="游戏服务器" -- bash -c "rebar3 shell --config config/game.config -s game_app start; exec bash" 2>/dev/null || \
    xterm -title "游戏服务器" -e "cd '$PROJECT_ROOT' && rebar3 shell --config config/game.config -s game_app start; exec bash" 2>/dev/null || \
    (echo "无法打开新终端，在当前终端启动游戏服务器..." && rebar3 shell --config config/game.config -s game_app start)
    
    echo -e "${GREEN}所有服务器启动完成！${NC}"
    sleep 3
}

# 主循环
main() {
    while true; do
        show_menu
        read -p "请输入选择 (1-5): " choice
        
        case $choice in
            1)
                start_login
                ;;
            2)
                start_cross
                ;;
            3)
                start_game
                ;;
            4)
                start_all
                ;;
            5)
                echo
                echo -e "${GREEN}感谢使用！${NC}"
                exit 0
                ;;
            *)
                echo -e "${RED}无效选择，请重新输入！${NC}"
                sleep 2
                ;;
        esac
    done
}

# 检查rebar3是否安装
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}错误: 未找到rebar3命令，请确保已安装Erlang和rebar3${NC}"
    exit 1
fi

# 启动主程序
main