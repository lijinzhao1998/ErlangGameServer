-module(role_timer_example).
-export([start_health_regen/1, start_exp_gain/1, start_daily_reward/1, start_weekly_task/1]).

%% 示例：玩家生命值恢复定时器
%% 每30秒恢复1点生命值
start_health_regen(RoleId) ->
    role_timer:start_timer(RoleId, ?MODULE, health_regen_callback, [], 30000).

%% 示例：玩家经验值获取定时器
%% 每60秒获得10点经验值，重复100次
start_exp_gain(RoleId) ->
    role_timer:start_timer(RoleId, ?MODULE, exp_gain_callback, [10], 60000, 100).

%% 示例：每日奖励定时器
%% 每24小时执行一次
start_daily_reward(RoleId) ->
    role_timer:start_timer(RoleId, ?MODULE, daily_reward_callback, [], 86400000).

%% 示例：每周任务定时器
%% 每7天执行一次
start_weekly_task(RoleId) ->
    role_timer:start_timer(RoleId, ?MODULE, weekly_task_callback, [], 604800000).

%% 回调函数实现
%% 注意：这些函数会自动接收RoleId作为第一个参数

%% 生命值恢复回调
health_regen_callback(RoleId) ->
    logger:log_info("玩家 ~p 生命值恢复 +1", [RoleId]),
    %% 这里可以调用实际的游戏逻辑
    %% 例如：player_manager:add_health(RoleId, 1),
    ok.

%% 经验值获取回调
exp_gain_callback(RoleId, ExpAmount) ->
    logger:log_info("玩家 ~p 获得经验值 +~p", [RoleId, ExpAmount]),
    %% 这里可以调用实际的游戏逻辑
    %% 例如：player_manager:add_exp(RoleId, ExpAmount),
    ok.

%% 每日奖励回调
daily_reward_callback(RoleId) ->
    logger:log_info("玩家 ~p 领取每日奖励", [RoleId]),
    %% 这里可以调用实际的游戏逻辑
    %% 例如：reward_manager:give_daily_reward(RoleId),
    ok.

%% 每周任务回调
weekly_task_callback(RoleId) ->
    logger:log_info("玩家 ~p 刷新每周任务", [RoleId]),
    %% 这里可以调用实际的游戏逻辑
    %% 例如：task_manager:refresh_weekly_tasks(RoleId),
    ok. 