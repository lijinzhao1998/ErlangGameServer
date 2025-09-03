```markdown
# Erlang 多服架构示例（从 0 开始）

目标
- 搭建一个基础的 Erlang 服务器架构：
  - login 服务（负责认证、分配会话、路由信息）
  - 多个 cross 服务（跨服网关 / 转发 / 会话管理）
  - 每个 cross 下可以有多个 game 服务（具体游戏逻辑）
- 提供可运行的最小示例代码（监听 TCP，简单握手），便于扩展到集群、认证、分布式消息转发与持久化。

项目结构（示例）
- apps/
  - common/      : 公共协议/工具
  - login/       : 登录服（接受玩家连接、鉴权、分配 cross）
  - cross/       : 跨服服（玩家路由、跨服消息转发）
  - game/        : 游戏服（具体游戏逻辑）

设计思路（简要）
1. 每个服务作为一个独立 OTP application，且每个服务可部署为多个节点（Erlang 分布式节点）。
2. login 负责用户鉴权并返回可连接的 cross 节点信息（或通过分配 token）。
3. cross 作为玩家连接的入口，维护会话、向 game 转发请求（RPC / 消息协议）。
4. game 处理游戏逻辑，建议使用基于进程的玩家会话（每个玩家一个 Erlang 进程）。
5. 使用全局注册或 gproc、pg2、global 等进行进程发现；Mnesia 或外部 DB（Postgres/Redis）做持久化。
6. 采用轻量协议（JSON / 二进制自定义包）通过 TCP/SSL 传输。示例用 line-based TCP 简化。

如何运行示例（最小化）
1. 安装 rebar3。
2. 编译：
   - rebar3 compile
3. 直接启动单节点（login）测试监听：
   - erl -pa _build/default/lib/*/ebin -sname login@127.0.0.1 -setcookie secret -eval "application:start(login), io:format(\"Login started~n\"), init:stop()."
   （上面示例会启动并退出；也可以进入 shell 手动 start）
4. 推荐交互式测试：
   - erl -pa _build/default/lib/*/ebin -sname shell@127.0.0.1 -setcookie secret
   - 在 erl shell 里启动： application:start(login).
5. 测试连接：
   - 使用 telnet: telnet 127.0.0.1 5000
   - 发送 "hello" 或 "login alice 123" 等，会收到示例应答。

## 新增功能

### 日志系统
- 使用Debugger Error Log格式，包含完整的调试信息
- 支持堆栈跟踪和调用栈信息
- 按类型和日期自动分文件存储
- 同时输出到控制台和文件

### ID生成器系统
- 集中管理所有类型的唯一ID生成
- 支持角色、会话、物品、公会等多种ID类型
- 自动从数据库加载现有最大ID，避免冲突

### 数据库系统
- 支持玩家、物品、公会等数据管理
- 自动表结构初始化
- 集成ID生成器

### Git版本控制管理
- **Git管理工具** - 完整的Git操作功能
- **Git快速操作** - 常用Git工作流程
- **Git工作流程** - Git Flow工作流程管理
- 支持分支管理、提交、推送、合并等操作
- 提供图形化菜单界面，简化Git操作

## 快速开始

### 使用管理脚本（推荐）

1. 启动主菜单：
```bash
scripts/start.bat
```

2. 选择相应功能：
   - 编译项目
   - 启动服务器
   - 管理Git版本控制
   - 数据库管理
   - 热更新等

### 手动操作

1. 编译项目：
```bash
rebar3 compile
```

2. 启动Erlang shell：
```bash
rebar3 shell
```

3. 测试系统功能：
```erlang
test_system:demo().
```

4. 单独测试日志系统：
```erlang
test_logger:test().
```

### Git版本控制

1. 通过主菜单访问Git工具：
   - 选择 `7` 进入Git管理工具
   - 选择 `8` 进入Git快速操作
   - 选择 `9` 进入Git工作流程

2. 直接运行Git脚本：
```bash
scripts/git_manager.bat      # Git管理工具
scripts/git_quick.bat        # Git快速操作
scripts/git_workflow.bat     # Git工作流程
```

## 下一步建议
- 把 login 的鉴权替换为真实 DB（Postgres / Mnesia）。
- cross 实现会话迁移与心跳、断线重连处理。
- game 实现玩家进程（gen_server），利用 pg 或 gproc 做分布式发现。
- 使用 SSL（ssl:listen）和消息压缩、二进制协议提升性能。
- 增加监控（prometheus、observer）、日志聚合、CI/CD、容器化部署。

下面提供一个可直接运行的最小代码骨架，包含 login/cross/game 的应用骨架与一个简单的 TCP listener（line-based）。
```