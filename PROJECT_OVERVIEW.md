# Erlang游戏服务器项目总览

这是一个基于Erlang/OTP构建的完整游戏服务器系统，包含多个功能模块和系统集成。

## 🏗️ 系统架构

```
ErlangGameServer/
├── apps/
│   ├── game/              # 游戏服务器核心
│   ├── login/             # 登录服务器
│   ├── cross/             # 跨服服务器
│   ├── https_api/         # HTTPS API服务器
│   └── rpc/               # RPC消息传输系统
├── scripts/               # 启动和管理脚本
└── docs/                  # 项目文档
```

## 🚀 核心功能模块

### 1. 游戏服务器 (Game Server)
- **位置**: `apps/game/`
- **功能**: 游戏逻辑处理、玩家管理、游戏状态管理
- **特性**: 
  - 玩家进程管理
  - 游戏路由系统
  - 动态监督者架构
  - RPC服务集成

### 2. 登录服务器 (Login Server)
- **位置**: `apps/login/`
- **功能**: 用户认证、会话管理、登录验证
- **特性**:
  - 用户认证系统
  - 会话管理
  - 安全验证

### 3. 跨服服务器 (Cross Server)
- **位置**: `apps/cross/`
- **功能**: 跨服通信、数据同步、跨服活动
- **特性**:
  - 跨服连接管理
  - 数据同步机制
  - 跨服活动支持
  - 连接池管理

### 4. HTTPS API服务器
- **位置**: `apps/https_api/`
- **功能**: 后台管理接口、SDK集成、RESTful API
- **特性**:
  - SDK认证接口
  - 后台管理API
  - SSL/TLS支持
  - JSON API设计
  - 令牌认证机制

### 5. RPC消息传输系统
- **位置**: `apps/rpc/`
- **功能**: 服务器间通信、服务发现、负载均衡
- **特性**:
  - 服务注册与发现
  - 同步/异步调用
  - 连接管理和监控
  - 心跳检测
  - 错误处理和重试

## 🔌 系统集成

### 启动方式

#### 1. 仅启动RPC系统
```bash
# Linux/Mac
./start_rpc_demo.sh

# Windows
start_rpc_demo.bat
```

#### 2. 仅启动HTTPS API
```bash
# Linux/Mac
./start_https_api.sh

# Windows
start_https_api.bat
```

#### 3. 启动完整集成系统
```bash
# Linux/Mac
./start_integrated_system.sh

# Windows
start_integrated_system.bat
```

### 端口配置

| 服务 | 端口 | 协议 | 说明 |
|------|------|------|------|
| RPC服务器 | 8888 | TCP | 服务器间通信 |
| HTTPS API | 8443 | HTTPS | 后台管理和SDK接口 |
| 游戏服务器 | 动态 | TCP | 游戏逻辑处理 |

## 📡 通信架构

### 内部通信 (RPC)
```
游戏服务器 ←→ RPC系统 ←→ 登录服务器
     ↑           ↑           ↑
     └─── 跨服服务器 ───┘
```

### 外部接口 (HTTPS API)
```
客户端/SDK ←→ HTTPS API ←→ 游戏服务器
后台管理  ←→ HTTPS API ←→ 游戏服务器
```

## 🧪 测试和演示

### RPC系统测试
```erlang
% 运行所有测试
test_rpc:test_all().

% 运行演示
test_rpc:demo().

% 查看服务状态
rpc_api:get_stats().
```

### HTTPS API测试
```bash
# SDK登录测试
curl -X POST https://localhost:8443/api/v1/sdk/login \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"123456"}' \
  -k

# 获取系统统计
curl -X GET https://localhost:8443/api/v1/admin/stats \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -k
```

### 游戏服务器测试
```erlang
% 查看游戏状态
game_rpc_handler:get_game_state().

% 创建测试玩家
game_rpc_handler:create_player(#{name => "测试玩家"}).

% 获取所有玩家
game_rpc_handler:get_all_players().
```

## 🔧 开发指南

### 添加新服务

1. **创建服务模块**
```erlang
-module(my_service).
-behaviour(gen_server).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % 注册到RPC系统
    rpc_api:register_service("my_service", self()),
    {ok, #{}}.
```

2. **实现RPC接口**
```erlang
handle_call({my_method, Params}, _From, State) ->
    % 处理业务逻辑
    Result = process_my_method(Params),
    {reply, {ok, Result}, State}.
```

3. **集成到应用**
```erlang
% 在应用启动时启动服务
my_service:start_link().
```

### 扩展API接口

1. **在https_handler.erl中添加路由**
2. **创建对应的处理模块**
3. **实现业务逻辑**
4. **添加测试用例**

## 📊 监控和维护

### 系统监控
- RPC连接状态监控
- 服务健康检查
- 性能指标统计
- 错误日志记录

### 维护操作
- 服务重启和恢复
- 连接池管理
- 证书更新
- 配置热更新

## 🚀 部署说明

### 环境要求
- Erlang/OTP 24+
- OpenSSL (用于HTTPS)
- 支持的操作系统: Linux, macOS, Windows

### 部署步骤
1. 安装Erlang/OTP
2. 克隆项目代码
3. 配置环境变量
4. 启动目标服务
5. 验证系统状态

### 生产环境配置
- 启用SSL/TLS
- 配置防火墙规则
- 设置监控告警
- 配置日志轮转
- 设置备份策略

## 🔮 未来规划

### 短期目标
- [ ] 完善错误处理
- [ ] 添加更多测试用例
- [ ] 优化性能配置
- [ ] 完善文档

### 长期目标
- [ ] 支持集群部署
- [ ] 添加消息队列
- [ ] 实现分布式锁
- [ ] 支持微服务架构
- [ ] 添加链路追踪

## 📞 技术支持

### 常见问题
1. **端口冲突**: 检查端口是否被占用
2. **证书问题**: 确保OpenSSL正确安装
3. **连接失败**: 检查网络配置和防火墙
4. **性能问题**: 调整连接池和超时配置

### 调试技巧
- 使用Erlang shell进行交互式调试
- 查看系统日志和错误信息
- 使用监控接口检查系统状态
- 逐步启动各个模块进行问题定位

这个系统为游戏开发提供了完整的服务器端解决方案，支持高并发、高可用的游戏服务架构。 