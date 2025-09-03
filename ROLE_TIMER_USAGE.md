# Role Timer 功能使用说明

## 概述

`role_timer` 是一个专为游戏服务器设计的玩家级定时器管理模块，允许为每个玩家设置独立的定时器，定期执行指定的回调函数。

## 主要特性

- **玩家级管理**: 每个玩家可以拥有多个独立的定时器
- **灵活的时间间隔**: 支持从1秒到24小时的定时间隔
- **重复次数控制**: 支持有限次数重复或无限重复
- **自动回调**: 定时器到期时自动调用指定模块的函数
- **高效管理**: 使用智能调度算法，最小化系统开销

## 基本用法

### 1. 启动定时器

```erlang
%% 基本用法：无限重复
role_timer:start_timer(RoleId, Module, Function, Args, Interval)

%% 高级用法：指定重复次数
role_timer:start_timer(RoleId, Module, Function, Args, Interval, RepeatCount)
```

**参数说明:**
- `RoleId`: 玩家角色ID
- `Module`: 回调函数所在模块
- `Function`: 回调函数名
- `Args`: 传递给回调函数的参数列表
- `Interval`: 时间间隔（毫秒）
- `RepeatCount`: 重复次数（-1表示无限重复）

### 2. 停止定时器

```erlang
role_timer:stop_timer(RoleId, TimerId)
```

### 3. 查询玩家定时器

```erlang
role_timer:get_timers(RoleId)
```

## 使用示例

### 示例1: 生命值恢复定时器

```erlang
%% 每30秒恢复1点生命值
role_timer:start_timer(1001, player_manager, add_health, [1], 30000).

%% 回调函数实现
add_health(RoleId, Amount) ->
    %% 这里实现实际的游戏逻辑
    player_manager:add_health(RoleId, Amount),
    ok.
```

### 示例2: 经验值获取定时器

```erlang
%% 每60秒获得10点经验值，重复100次
role_timer:start_timer(1001, player_manager, add_exp, [10], 60000, 100).

%% 回调函数实现
add_exp(RoleId, Amount) ->
    player_manager:add_exp(RoleId, Amount),
    ok.
```

### 示例3: 每日奖励定时器

```erlang
%% 每24小时执行一次
role_timer:start_timer(1001, reward_manager, give_daily_reward, [], 86400000).

%% 回调函数实现
give_daily_reward(RoleId) ->
    reward_manager:give_daily_reward(RoleId),
    ok.
```

## 回调函数规范

**重要**: 回调函数会自动接收 `RoleId` 作为第一个参数！

```erlang
%% 错误写法
my_callback(Param1, Param2) ->
    ok.

%% 正确写法
my_callback(RoleId, Param1, Param2) ->
    %% RoleId 会自动传入
    ok.
```

## 时间间隔限制

- **最小间隔**: 1秒 (1000毫秒)
- **最大间隔**: 24小时 (86400000毫秒)

## 系统集成

`role_timer` 模块已经集成到 `cross_sup` 监督者中，会在系统启动时自动启动。

## 性能考虑

- 使用高效的调度算法，最小化CPU占用
- 支持大量玩家同时使用定时器
- 自动清理已完成的定时器
- 智能的检查间隔，避免频繁唤醒

## 错误处理

定时器执行失败时会记录错误日志，但不会影响其他定时器的正常运行。

## 测试

使用提供的测试脚本验证功能：

```erlang
%% 编译并运行测试
c(test_role_timer).
test_role_timer:test().
```

## 注意事项

1. 回调函数必须是导出的（exported）
2. 回调函数参数数量必须匹配（包括自动添加的RoleId）
3. 长时间运行的定时器回调可能会影响系统性能
4. 建议在回调函数中添加适当的错误处理
5. 系统重启后定时器状态会丢失，需要重新设置

## 扩展功能

可以根据需要扩展以下功能：
- 定时器持久化存储
- 定时器优先级管理
- 定时器组管理
- 定时器统计信息
- 定时器热更新 