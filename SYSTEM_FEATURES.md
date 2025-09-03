# Erlang游戏服务器系统功能说明

## 新增功能概述

本系统新增了以下核心功能：

### 1. 日志系统 (Logger)

**功能特点：**
- 按日志类型分类存储（info, warning, error, debug）
- 按日期自动分文件存储
- 支持带参数的日志记录
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
logger:log(error, ?MODULE, "带参数的日志: ~p", [test_data]).
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

### 2. MDB数据库系统

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
2. **MDB** - 数据库系统
3. **PlayerAuth** - 玩家认证
4. **DynamicSupervisor** - 连接管理
5. **CrossListener** - 网络监听

## 测试系统

使用 `test_system` 模块可以测试所有功能：

```erlang
% 测试所有功能
test_system:test_all().

% 单独测试各个模块
test_system:test_logger().
test_system:test_mdb().
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