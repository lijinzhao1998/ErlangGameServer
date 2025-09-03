-module(test_role_timer).
-export([test/0, test_health_regen/0, test_exp_gain/0, test_daily_reward/0, test_weekly_task/0]).

test() ->
    io:format("=== Role Timer 功能测试 ===~n"),
    
    %% 测试生命值恢复定时器
    test_health_regen(),
    
    %% 测试经验值获取定时器
    test_exp_gain(),
    
    %% 测试每日奖励定时器
    test_daily_reward(),
    
    %% 测试每周任务定时器
    test_weekly_task(),
    
    %% 等待一段时间观察定时器执行
    io:format("~n等待10秒观察定时器执行...~n"),
    timer:sleep(10000),
    
    %% 查看玩家1001的定时器状态
    case role_timer:get_timers(1001) of
        {ok, Timers} ->
            io:format("玩家1001的定时器状态:~n"),
            lists:foreach(fun({TimerId, TimerInfo}) ->
                io:format("  ~s: ~p:~p/~p 间隔 ~pms 剩余次数 ~p~n", 
                         [TimerId, 
                          maps:get(module, TimerInfo),
                          maps:get(function, TimerInfo),
                          length(maps:get(args, TimerInfo)),
                          maps:get(interval, TimerInfo),
                          maps:get(repeat_count, TimerInfo)])
            end, Timers);
        {error, Reason} ->
            io:format("获取定时器失败: ~p~n", [Reason])
    end,
    
    io:format("~n测试完成！~n").

test_health_regen() ->
    io:format("~n1. 测试生命值恢复定时器 (每30秒执行)...~n"),
    case role_timer:start_timer(1001, role_timer_example, start_health_regen, [], 30000) of
        {ok, TimerId} ->
            io:format("   启动成功，定时器ID: ~s~n", [TimerId]);
        {error, Reason} ->
            io:format("   启动失败: ~p~n", [Reason])
    end.

test_exp_gain() ->
    io:format("~n2. 测试经验值获取定时器 (每60秒执行，重复100次)...~n"),
    case role_timer:start_timer(1001, role_timer_example, start_exp_gain, [10], 60000, 100) of
        {ok, TimerId} ->
            io:format("   启动成功，定时器ID: ~s~n", [TimerId]);
        {error, Reason} ->
            io:format("   启动失败: ~p~n", [Reason])
    end.

test_daily_reward() ->
    io:format("~n3. 测试每日奖励定时器 (每24小时执行)...~n"),
    case role_timer:start_timer(1001, role_timer_example, start_daily_reward, [], 86400000) of
        {ok, TimerId} ->
            io:format("   启动成功，定时器ID: ~s~n", [TimerId]);
        {error, Reason} ->
            io:format("   启动失败: ~p~n", [Reason])
    end.

test_weekly_task() ->
    io:format("~n4. 测试每周任务定时器 (每7天执行)...~n"),
    case role_timer:start_timer(1001, role_timer_example, start_weekly_task, [], 604800000) of
        {ok, TimerId} ->
            io:format("   启动成功，定时器ID: ~s~n", [TimerId]);
        {error, Reason} ->
            io:format("   启动失败: ~p~n", [Reason])
    end. 