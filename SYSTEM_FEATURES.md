# Erlang游戏服务器系统功能说明

## 新增功能概述

本系统新增了以下核心功能：

### 1. 日志系统 (Logger)

**功能特点：**
- 使用Debugger Error Log格式，包含完整的调试信息
- 按日志类型分类存储（info, warning, error, debug）
- 按日期自动分文件存储
- 支持带参数的日志记录
- 支持堆栈跟踪和调用栈信息
- 同时输出到控制台和文件
- 异步写入，不影响性能

**使用方法：**
```erlang
% 基本日志
logger:log_info("这是一条信息日志"),
logger:log_warning("这是一条警告日志"),
logger:log_error("这是一条错误日志"),
logger:log_debug("这是一条调试日志"),

% 带模块名的日志
logger:log(info, ?MODULE, "带模块名的日志"),

% 带参数的日志
logger:log(error, ?MODULE, "带参数的日志: ~p", [test_data]),

% 带堆栈跟踪的日志
logger:log_with_stack(error, ?MODULE, "异常信息", StackTrace),

% 带参数和堆栈的日志
logger:log_with_stack(warning, ?MODULE, "警告: ~p", [Data], StackTrace).
```

**日志格式示例：**
```
=ERROR REPORT==== 2024-01-01 12:34:56 ===
Process: <0.123.0> (logger)
Module: test_system
Level: error
Message: 捕获到异常: test_exception
Location: apps/cross/src/test_system.erl:45
Call Stack:
  test_system:test_logger/0 at apps/cross/src/test_system.erl:45
  test_system:test_all/0 at apps/cross/src/test_system.erl:15
```

**日志文件结构：**
```
logs/
├── info/
│   ├── 2024-01-01.log
│   └── 2024-01-02.log
├── warning/
├── error/
└── debug/
```

### 2. ID生成器系统

**功能特点：**
- 集中管理所有类型的唯一ID生成
- 支持多种ID类型（角色、会话、物品、公会等）
- 自动从数据库加载现有最大ID，避免冲突
- 按类型分配ID范围，确保唯一性
- 支持批量生成和ID重置

**使用方法：**
```erlang
% 生成各种类型的ID
{ok, RoleId} = id_generator:generate_role_id(),
{ok, SessionId} = id_generator:generate_session_id(),
{ok, ItemId} = id_generator:generate_item_id(),
{ok, GuildId} = id_generator:generate_guild_id(),

% 使用通用接口生成ID
{ok, Id} = id_generator:generate_id(role_id),

% 获取ID状态
{ok, Status} = id_generator:get_id_status().
```

**ID范围分配：**
- 角色ID：1000+ (玩家角色)
- 会话ID：10000+ (用户会话)
- 物品ID：20000+ (游戏物品)
- 订单ID：30000+ (交易订单)
- 公会ID：40000+ (游戏公会)
- 邮件ID：50000+ (系统邮件)
- 聊天ID：60000+ (聊天记录)
- 交易ID：70000+ (交易记录)
- 战斗ID：80000+ (战斗记录)
- 任务ID：90000+ (游戏任务)

### 3. MDB数据库系统

**功能特点：**
- 支持SQL查询和执行
- 玩家数据管理（增删改查）
- 自动表结构初始化
- 事务支持（可扩展）

**使用方法：**
```erlang
% 执行SQL
mdb:execute("INSERT INTO players (role_id, username) VALUES (?, ?)", [1001, "testuser"]),

% 查询数据
{ok, Result} = mdb:query("SELECT * FROM players WHERE role_id = ?", [1001]),

% 玩家数据操作
{ok, Player} = mdb:get_player(1001),
{ok, created} = mdb:create_player(PlayerData),
{ok, updated} = mdb:update_player(PlayerData),
{ok, deleted} = mdb:delete_player(1001).
```

### 3. 玩家认证系统

**功能特点：**
- 用户注册和登录
- 会话管理
- 密码哈希验证
- 自动会话超时

**使用方法：**
```erlang
% 用户注册
{ok, #{role_id := RoleId, username := Username}} = 
    player_auth:register("newuser", "password", "昵称"),

% 用户登录
{ok, #{session_id := SessionId, role_id := RoleId}} = 
    player_auth:login("username", "password"),

% 会话验证
{ok, SessionData} = player_auth:authenticate(SessionId),

% 用户登出
ok = player_auth:logout(SessionId).
```

### 4. 增强的玩家进程

**功能特点：**
- 自动数据库加载
- 状态管理和持久化
- 自动保存机制
- 经验升级系统
- 金币管理

**使用方法：**
```erlang
% 启动玩家进程
{ok, Pid} = game_player:start_link(RoleId),

% 获取玩家状态
{ok, State} = game_player:get_state(RoleId),

% 更新玩家属性
game_player:add_exp(RoleId, 500),
game_player:add_gold(RoleId, 1000),
game_player:set_nickname(RoleId, "新昵称"),

% 手动保存状态
{ok, saved} = game_player:save_state(RoleId),

% 停止玩家进程
ok = game_player:stop(RoleId).
```

## 数据库表结构

### players表
```sql
CREATE TABLE players (
    role_id INTEGER PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    nickname VARCHAR(50),
    level INTEGER DEFAULT 1,
    exp INTEGER DEFAULT 0,
    gold INTEGER DEFAULT 0,
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_login_time TIMESTAMP,
    status INTEGER DEFAULT 1
);
```

### player_sessions表
```sql
CREATE TABLE player_sessions (
    session_id VARCHAR(100) PRIMARY KEY,
    role_id INTEGER NOT NULL,
    username VARCHAR(50) NOT NULL,
    login_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expire_time TIMESTAMP NOT NULL,
    ip_address VARCHAR(45),
    user_agent TEXT,
    FOREIGN KEY (role_id) REFERENCES players(role_id)
);
```

## 启动顺序

系统启动时会按以下顺序启动各个模块：

1. **Logger** - 日志系统
2. **IdGenerator** - ID生成器
3. **MDB** - 数据库系统
4. **PlayerAuth** - 玩家认证
5. **DynamicSupervisor** - 连接管理
6. **CrossListener** - 网络监听

## 测试系统

使用 `test_system` 模块可以测试所有功能：

```erlang
% 测试所有功能
test_system:test_all().

% 单独测试各个模块
test_system:test_logger().
test_system:test_id_generator().
test_system:test_mdb().
test_system:test_items_and_guilds().
test_system:test_auth().
test_system:test_player().

% 运行演示
test_system:demo().
```

## 配置说明

### 日志配置
- 日志目录：`logs/`
- 自动分文件：按日期和类型
- 日志级别：info, warning, error, debug

### 数据库配置
- 数据库文件：`game_data.db`
- 自动初始化：启动时自动创建表结构
- 连接池：可配置连接数量

### 玩家进程配置
- 自动保存间隔：5分钟
- 经验升级：每级1000经验
- 状态持久化：进程终止时自动保存

## 扩展建议

1. **实际数据库驱动**：替换模拟的数据库操作为真实的SQLite/MySQL驱动
2. **密码安全**：使用bcrypt等安全的密码哈希算法
3. **缓存系统**：添加Redis缓存提高性能
4. **监控系统**：集成Prometheus等监控工具
5. **配置管理**：使用配置文件管理各种参数

## 注意事项

1. 当前MDB模块使用模拟实现，生产环境需要替换为真实数据库驱动
2. 密码哈希使用简单的MD5，生产环境应使用更安全的算法
3. 日志文件会持续增长，建议配置日志轮转
4. 玩家进程会自动保存，但建议定期备份数据库 