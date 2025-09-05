#!/bin/bash

# 设置UTF-8编码
export LANG=zh_CN.UTF-8

# 获取脚本所在目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 颜色定义
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}        Erlang游戏服务器${NC}"
echo -e "${BLUE}========================================${NC}"
echo
echo "正在启动管理界面..."
echo

# 切换到脚本目录并运行主菜单
cd "$SCRIPT_DIR"
./main_menu.sh