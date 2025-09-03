# Erlang游戏服务器安装和配置说明

## 问题解决

### 1. 语法错误已修复

您遇到的语法错误已经修复：
- `apps/game/src/game.app.src` - 已修复
- `apps/login/src/login.app.src` - 已修复
- `apps/cross/src/cross.app.src` - 语法正确

### 2. 安装 Erlang 和 rebar3

#### 安装 Erlang
1. 访问 [Erlang 官网](https://www.erlang.org/downloads)
2. 下载适合您系统的 Erlang 安装包
3. 安装完成后，确保 `erl` 命令可用

#### 安装 rebar3
1. 访问 [rebar3 官网](https://www.rebar3.org/)
2. 下载适合您系统的 rebar3
3. 或者使用以下命令安装：

**Windows (使用 Chocolatey):**
```bash
choco install rebar3
```

**Windows (手动安装):**
1. 下载 rebar3.exe
2. 将其放在 PATH 环境变量包含的目录中
3. 或者放在项目根目录下

**Linux/macOS:**
```bash
curl -fsSL https://raw.githubusercontent.com/erlang/rebar3/master/rebar3 -o rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

### 3. 验证安装

安装完成后，在命令行中验证：

```bash
# 检查 Erlang 版本
erl -version

# 检查 rebar3 版本
rebar3 version
```

### 4. 编译项目

安装完成后，您可以使用以下命令编译项目：

```bash
# 清理项目
rebar3 clean

# 编译项目
rebar3 compile

# 运行测试
rebar3 test

# 启动 shell
rebar3 shell
```

### 5. 使用管理脚本

项目提供了完整的管理脚本：

```bash
# 启动主菜单
scripts/start.bat

# 或者直接使用特定功能
scripts/main_menu.bat
scripts/start_server.bat
scripts/server_manager.bat
scripts/hot_reload.bat
scripts/database_manager.bat
scripts/quick_start.bat
```

### 6. 常见问题

#### 问题1: rebar3 命令未找到
**解决方案:** 确保 rebar3 已正确安装并添加到 PATH 环境变量中

#### 问题2: Erlang 版本不兼容
**解决方案:** 使用 Erlang/OTP 24 或更高版本

#### 问题3: 端口被占用
**解决方案:** 检查配置文件中的端口设置，确保端口未被其他服务占用

### 7. 开发环境配置

#### 推荐的开发工具
- **Erlang/OTP**: 最新稳定版本
- **rebar3**: 最新版本
- **编辑器**: VS Code + Erlang 插件，或 IntelliJ IDEA + Erlang 插件

#### 环境变量设置
```bash
# Windows
set ERLANG_HOME=C:\Program Files\Erlang OTP
set PATH=%ERLANG_HOME%\bin;%PATH%

# Linux/macOS
export ERLANG_HOME=/usr/local/lib/erlang
export PATH=$ERLANG_HOME/bin:$PATH
```

### 8. 下一步

1. 安装 Erlang 和 rebar3
2. 验证安装是否成功
3. 编译项目
4. 使用管理脚本启动服务器
5. 开始开发和测试

### 9. 技术支持

如果遇到其他问题，请：
1. 检查错误日志
2. 查看项目文档
3. 联系开发团队

---

**注意:** 确保在运行任何脚本之前，Erlang 和 rebar3 已正确安装并配置。 