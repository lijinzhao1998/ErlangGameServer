-module(test_logger).
-export([test/0]).

test() ->
    %% 启动日志系统
    logger:start_link(),
    
    %% 测试基本日志
    io:format("=== 测试基本日志 ===~n"),
    logger:log_info("这是一条信息日志"),
    logger:log_warning("这是一条警告日志"),
    logger:log_error("这是一条错误日志"),
    logger:log_debug("这是一条调试日志"),
    
    %% 测试带模块名的日志
    io:format("~n=== 测试带模块名的日志 ===~n"),
    logger:log(info, ?MODULE, "带模块名的信息日志"),
    logger:log(error, ?MODULE, "带模块名的错误日志"),
    
    %% 测试带参数的日志
    io:format("~n=== 测试带参数的日志 ===~n"),
    logger:log(info, ?MODULE, "用户 ~s 登录成功，角色ID: ~p", ["testuser", 1001]),
    logger:log(warning, ?MODULE, "玩家 ~p 尝试访问未授权区域", [1001]),
    
    %% 测试带堆栈跟踪的日志
    io:format("~n=== 测试带堆栈跟踪的日志 ===~n"),
    try
        throw(test_exception)
    catch
        _:TestReason:TestStackTrace ->
            logger:log_with_stack(error, ?MODULE, "捕获到测试异常: ~p", [TestReason], TestStackTrace)
    end,
    
    %% 测试带参数的堆栈日志
    io:format("~n=== 测试带参数的堆栈日志 ===~n"),
    logger:log_with_stack(warning, ?MODULE, "警告消息: ~p", [warning_data], []),
    
    %% 测试异常情况
    io:format("~n=== 测试异常情况 ===~n"),
    try
        lists:nth(10, [1, 2, 3])
    catch
        _:ListReason:ListStackTrace ->
            logger:log_with_stack(error, ?MODULE, "列表索引越界: ~p", [ListReason], ListStackTrace)
    end,
    
    io:format("~n=== 日志测试完成 ===~n"),
    io:format("请检查logs目录中的日志文件和控制台输出~n"),
    
    %% 停止日志系统
    logger:stop(). 