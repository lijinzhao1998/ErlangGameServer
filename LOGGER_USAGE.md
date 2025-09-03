# 日志系统使用说明

## 概述

本日志系统使用Debugger Error Log格式，提供完整的调试信息，包括：
- 时间戳（精确到毫秒）
- 进程ID和名称
- 模块名和调用位置
- 日志级别
- 消息内容
- 调用栈信息
- 堆栈跟踪（可选）

## 基本用法

### 1. 简单日志
```erlang
logger:log_info("这是一条信息日志"),
logger:log_warning("这是一条警告日志"),
logger:log_error("这是一条错误日志"),
logger:log_debug("这是一条调试日志").
```

### 2. 带模块名的日志
```erlang
logger:log(info, ?MODULE, "带模块名的日志"),
logger:log(error, ?MODULE, "带参数的日志: ~p", [error_data]).
```

### 3. 带堆栈跟踪的日志
```erlang
try
    some_risky_operation()
catch
    _:Reason:StackTrace ->
        logger:log_with_stack(error, ?MODULE, "操作失败: ~p", [Reason], StackTrace)
end.
```

### 4. 带参数的堆栈日志
```erlang
logger:log_with_stack(warning, ?MODULE, "警告消息: ~p", [warning_data], []).
```

## 日志格式示例

```
=ERROR REPORT==== 2024-01-01 12:34:56 ===
Process: <0.123.0> (logger)
Module: test_system
Level: error
Message: 捕获到异常: test_exception
Location: apps/cross/src/test_system.erl:55
Call Stack:
  test_system:test_logger/0 at apps/cross/src/test_system.erl:55
  test_system:test_all/0 at apps/cross/src/test_system.erl:15
```

## 日志文件结构

```
logs/
├── info/           # 信息日志
│   ├── 2024-01-01.log
│   └── 2024-01-02.log
├── warning/        # 警告日志
├── error/          # 错误日志
└── debug/          # 调试日志
```

## 特性

1. **自动分类**: 按日志级别自动分类存储
2. **日期分文件**: 按日期自动创建新的日志文件
3. **双重输出**: 同时输出到控制台和文件
4. **异步写入**: 不影响应用性能
5. **完整上下文**: 包含进程、模块、位置等完整信息
6. **堆栈支持**: 支持异常堆栈跟踪

## 测试

### 运行完整测试
```erlang
test_system:demo().
```

### 单独测试日志系统
```erlang
test_logger:test().
```

### 测试特定功能
```erlang
test_system:test_logger().
```

## 配置

日志系统在启动时自动创建必要的目录结构，无需额外配置。

## 注意事项

1. 日志文件会持续增长，建议定期清理或配置日志轮转
2. 生产环境中可以根据需要调整日志级别
3. 堆栈跟踪会增加日志大小，请根据实际需求使用
4. 所有日志都会同时输出到控制台，便于开发调试 