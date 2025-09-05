#!/bin/bash

# 设置UTF-8编码
export LANG=zh_CN.UTF-8

# 获取脚本所在目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 显示主菜单
show_menu() {
    clear
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}        Erlang游戏服务器主菜单${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo
    echo "请选择要执行的操作:"
    echo
    echo "1. 启动服务器"
    echo "2. 服务器管理"
    echo "3. 热更新"
    echo "4. 数据库管理"
    echo "5. 一键启动所有服务器"
    echo "6. 编译项目"
    echo "7. Git管理工具"
    echo "8. Git快速操作"
    echo "9. Git工作流程"
    echo "10. 退出"
    echo
    echo -e "${BLUE}========================================${NC}"
}

# 启动服务器
start_server() {
    clear
    echo -e "${YELLOW}正在启动服务器管理界面...${NC}"
    cd "$SCRIPT_DIR"
    if [ -f "./start_server.sh" ]; then
        ./start_server.sh
    else
        echo -e "${RED}错误: 未找到 start_server.sh 文件${NC}"
        sleep 2
    fi
}

# 服务器管理
server_management() {
    clear
    echo -e "${YELLOW}正在启动服务器管理界面...${NC}"
    cd "$SCRIPT_DIR"
    if [ -f "./server_manager.sh" ]; then
        ./server_manager.sh
    else
        echo -e "${RED}错误: 未找到 server_manager.sh 文件${NC}"
        echo -e "${YELLOW}提示: 需要先创建 server_manager.sh 脚本${NC}"
        sleep 3
    fi
}

# 热更新
hot_reload() {
    clear
    echo -e "${YELLOW}正在启动热更新界面...${NC}"
    cd "$SCRIPT_DIR"
    if [ -f "./hot_reload.sh" ]; then
        ./hot_reload.sh
    else
        echo -e "${RED}错误: 未找到 hot_reload.sh 文件${NC}"
        echo -e "${YELLOW}提示: 需要先创建 hot_reload.sh 脚本${NC}"
        sleep 3
    fi
}

# 数据库管理
database_management() {
    clear
    echo -e "${YELLOW}正在启动数据库管理界面...${NC}"
    cd "$SCRIPT_DIR"
    if [ -f "./database_manager.sh" ]; then
        ./database_manager.sh
    else
        echo -e "${RED}错误: 未找到 database_manager.sh 文件${NC}"
        echo -e "${YELLOW}提示: 需要先创建 database_manager.sh 脚本${NC}"
        sleep 3
    fi
}

# 一键启动所有服务器
quick_start_all() {
    clear
    echo -e "${YELLOW}正在启动一键启动脚本...${NC}"
    cd "$SCRIPT_DIR"
    if [ -f "./quick_start.sh" ]; then
        ./quick_start.sh
    else
        echo -e "${RED}错误: 未找到 quick_start.sh 文件${NC}"
        echo -e "${YELLOW}提示: 需要先创建 quick_start.sh 脚本${NC}"
        sleep 3
    fi
}

# 编译项目
compile_project() {
    clear
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}           编译项目${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo
    echo -e "${YELLOW}正在编译项目...${NC}"
    
    # 切换到项目根目录
    PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
    cd "$PROJECT_ROOT"
    
    echo "当前目录: $(pwd)"
    echo
    
    echo -e "${YELLOW}执行清理...${NC}"
    rebar3 clean
    
    echo
    echo -e "${YELLOW}执行编译...${NC}"
    rebar3 compile
    
    if [ $? -eq 0 ]; then
        echo
        echo -e "${GREEN}========================================${NC}"
        echo -e "${GREEN}编译成功！${NC}"
        echo -e "${GREEN}========================================${NC}"
        echo
        echo -e "${GREEN}项目已成功编译，可以启动服务器了。${NC}"
    else
        echo
        echo -e "${RED}========================================${NC}"
        echo -e "${RED}编译失败！${NC}"
        echo
        echo -e "${RED}请检查错误信息并修复问题。${NC}"
    fi
    
    echo
    echo "按任意键返回主菜单..."
    read -n 1 -s
}

# Git管理工具
git_management() {
    clear
    echo -e "${YELLOW}正在启动Git管理工具...${NC}"
    cd "$SCRIPT_DIR"
    if [ -f "./git_manager.sh" ]; then
        ./git_manager.sh
    else
        echo -e "${RED}错误: 未找到 git_manager.sh 文件${NC}"
        echo -e "${YELLOW}提示: 需要先创建 git_manager.sh 脚本${NC}"
        sleep 3
    fi
}

# Git快速操作
git_quick() {
    clear
    echo -e "${YELLOW}正在启动Git快速操作...${NC}"
    cd "$SCRIPT_DIR"
    if [ -f "./git_quick.sh" ]; then
        ./git_quick.sh
    else
        echo -e "${RED}错误: 未找到 git_quick.sh 文件${NC}"
        echo -e "${YELLOW}提示: 需要先创建 git_quick.sh 脚本${NC}"
        sleep 3
    fi
}

# Git工作流程
git_workflow() {
    clear
    echo -e "${YELLOW}正在启动Git工作流程管理...${NC}"
    cd "$SCRIPT_DIR"
    if [ -f "./git_workflow.sh" ]; then
        ./git_workflow.sh
    else
        echo -e "${RED}错误: 未找到 git_workflow.sh 文件${NC}"
        echo -e "${YELLOW}提示: 需要先创建 git_workflow.sh 脚本${NC}"
        sleep 3
    fi
}

# 退出
exit_program() {
    clear
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}           感谢使用${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo
    echo -e "${GREEN}感谢使用Erlang游戏服务器管理脚本！${NC}"
    echo
    echo -e "${YELLOW}如有问题，请查看项目文档或联系开发团队。${NC}"
    echo
    echo "按任意键退出..."
    read -n 1 -s
    exit 0
}

# 主循环
main() {
    while true; do
        show_menu
        read -p "请输入选择 (1-10): " choice
        
        case $choice in
            1)
                start_server
                ;;
            2)
                server_management
                ;;
            3)
                hot_reload
                ;;
            4)
                database_management
                ;;
            5)
                quick_start_all
                ;;
            6)
                compile_project
                ;;
            7)
                git_management
                ;;
            8)
                git_quick
                ;;
            9)
                git_workflow
                ;;
            10)
                exit_program
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