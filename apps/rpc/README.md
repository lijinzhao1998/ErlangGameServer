# RPC 消息传输系统

这个模块为各个游戏服务器提供了完整的RPC（远程过程调用）消息传输封装，支持服务发现、负载均衡、连接管理和监控等功能。

## 🚀 功能特性

### 核心功能
- **服务注册与发现**: 自动服务注册和动态服务发现
- **同步/异步调用**: 支持同步调用、异步调用和通知
- **连接管理**: 自动连接管理、重连和心跳检测
- **负载均衡**: 智能节点选择和服务路由
- **监控统计**: 实时连接监控和性能统计

### 协议特性
- **二进制协议**: 高效的二进制消息格式
- **消息类型**: 支持请求、响应、通知和心跳
- **错误处理**: 完善的错误处理和重试机制
- **超时控制**: 可配置的超时和重试策略

## 📁 模块结构

```
apps/rpc/
├── src/
│   ├── rpc.app.src          # 应用配置
│   ├── rpc_app.erl          # 应用主模块
│   ├── rpc_sup.erl          # 监督者模块
│   ├── rpc_server.erl       # RPC服务器
│   ├── rpc_client.erl       # RPC客户端
│   ├── rpc_protocol.erl     # 协议处理
│   ├── rpc_registry.erl     # 服务注册表
│   ├── rpc_monitor.erl      # 监控模块
│   └── rpc_api.erl          # 高级API接口
├── test_rpc.erl             # 测试模块
└── README.md                # 使用说明
```

## 🔧 配置说明

在 `rpc.app.src` 中配置：

```erlang
{env, [
    {default_port, 8888},           % 默认RPC端口
    {heartbeat_interval, 30000},    % 心跳间隔(毫秒)
    {connection_timeout, 10000},    % 连接超时(毫秒)
    {max_connections, 1000},        % 最大连接数
    {enable_ssl, false},            % 是否启用SSL
    {ssl_port, 8889}                % SSL端口
]}
```

## 📖 使用方法

### 1. 启动RPC应用

```erlang
% 启动RPC应用
application:start(rpc).

% 启动RPC服务器监听器
rpc_server:start_listener(8888).
```

### 2. 服务注册

```erlang
% 注册当前节点上的服务
rpc_api:register_service("game_service", GamePid).

% 注册其他服务
rpc_api:register_service("login_service", LoginPid).
rpc_api:register_service("cross_service", CrossPid).
```

### 3. 服务调用

```erlang
% 同步调用
{ok, Result} = rpc_api:call("game_service", "get_player", PlayerId).

% 异步调用
rpc_api:cast("game_service", "update_player", PlayerData).

% 发送通知
rpc_api:notify("game_service", "player_logout").

% 批量调用
Calls = [
    {"game_service", "get_player", PlayerId1},
    {"game_service", "get_player", PlayerId2}
],
Results = rpc_api:batch_call(Calls, 5000).
```

### 4. 客户端连接

```erlang
% 连接到RPC服务器
rpc_api:connect("localhost", 8888).

% 检查连接状态
Connected = rpc_api:is_connected().

% 断开连接
rpc_api:disconnect().
```

### 5. 监控和统计

```erlang
% 获取连接统计
Stats = rpc_api:get_stats().

% 获取服务健康状态
Health = rpc_api:get_health().

% 列出所有服务
Services = rpc_api:list_services().
```

## 🔌 集成到现有服务器

### 游戏服务器集成

```erlang
% 在 game_app.erl 中
start(_StartType, _StartArgs) ->
    % 启动RPC应用
    application:start(rpc),
    
    % 注册游戏服务
    rpc_api:register_service("game_service", self()),
    
    % 启动游戏监督者
    game_sup:start_link().
```

### 登录服务器集成

```erlang
% 在 login_app.erl 中
start(_StartType, _StartArgs) ->
    % 启动RPC应用
    application:start(rpc),
    
    % 注册登录服务
    rpc_api:register_service("login_service", self()),
    
    % 启动登录监督者
    login_sup:start_link().
```

### 跨服服务器集成

```erlang
% 在 cross_app.erl 中
start(_StartType, _StartArgs) ->
    % 启动RPC应用
    application:start(rpc),
    
    % 注册跨服服务
    rpc_api:register_service("cross_service", self()),
    
    % 启动跨服监督者
    cross_sup:start_link().
```

## 🧪 测试

### 运行测试

```erlang
% 编译测试模块
c(test_rpc).

% 运行所有测试
test_rpc:test_all().

% 运行演示
test_rpc:demo().
```

### 测试场景

1. **服务注册测试**: 测试服务注册、发现和注销
2. **RPC调用测试**: 测试同步、异步和批量调用
3. **连接管理测试**: 测试连接建立和断开
4. **监控功能测试**: 测试统计信息和健康检查

## 🔒 安全特性

- **连接验证**: 支持连接验证和认证
- **消息加密**: 可选的SSL/TLS加密
- **访问控制**: 基于服务的访问控制
- **错误保护**: 防止恶意消息攻击

## 📊 性能特性

- **连接池**: 高效的连接池管理
- **消息缓存**: 智能消息缓存和路由
- **负载均衡**: 自动负载均衡和故障转移
- **监控告警**: 实时性能监控和告警

## 🚧 开发说明

### 扩展新功能

1. **添加新的消息类型**: 在 `rpc_protocol.erl` 中定义
2. **实现新的服务**: 创建服务模块并注册
3. **自定义协议**: 扩展协议处理逻辑
4. **添加监控指标**: 在 `rpc_monitor.erl` 中实现

### 错误处理

- 网络错误自动重连
- 服务不可用自动切换
- 超时请求自动取消
- 异常情况自动恢复

## 📝 注意事项

- 首次使用需要启动RPC应用
- 服务注册后需要等待注册完成
- 网络异常时会有自动重连机制
- 建议在生产环境中配置SSL
- 定期检查服务健康状态

## 🔮 未来计划

- [ ] 支持gRPC协议
- [ ] 添加消息压缩
- [ ] 实现分布式锁
- [ ] 支持流式调用
- [ ] 添加链路追踪
- [ ] 支持服务网格

这个RPC系统为你的游戏服务器提供了强大而灵活的消息传输能力，可以轻松实现服务器间的通信和协作。 