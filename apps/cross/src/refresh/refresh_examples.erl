-module(refresh_examples).
-export([
    %% 每日刷新示例
    daily_quest_refresh/0, daily_reward_refresh/0, daily_activity_refresh/0,
    
    %% 每周刷新示例
    weekly_quest_refresh/0, weekly_ranking_refresh/0, weekly_boss_refresh/0,
    
    %% 每月刷新示例
    monthly_ranking_refresh/0, monthly_vip_reward_refresh/0, monthly_season_refresh/0,
    
    %% 注册所有刷新处理器
    register_all_refresh/0
]).

%% 每日刷新处理器示例

%% 每日任务刷新
daily_quest_refresh() ->
    logger:log_info("执行每日任务刷新"),
    %% 这里实现实际的每日任务刷新逻辑
    %% 例如：quest_manager:refresh_daily_quests(),
    ok.

%% 每日奖励刷新
daily_reward_refresh() ->
    logger:log_info("执行每日奖励刷新"),
    %% 这里实现实际的每日奖励刷新逻辑
    %% 例如：reward_manager:refresh_daily_rewards(),
    ok.

%% 每日活动刷新
daily_activity_refresh() ->
    logger:log_info("执行每日活动刷新"),
    %% 这里实现实际的每日活动刷新逻辑
    %% 例如：activity_manager:refresh_daily_activities(),
    ok.

%% 每周刷新处理器示例

%% 每周任务刷新
weekly_quest_refresh() ->
    logger:log_info("执行每周任务刷新"),
    %% 这里实现实际的每周任务刷新逻辑
    %% 例如：quest_manager:refresh_weekly_quests(),
    ok.

%% 每周排行榜刷新
weekly_ranking_refresh() ->
    logger:log_info("执行每周排行榜刷新"),
    %% 这里实现实际的每周排行榜刷新逻辑
    %% 例如：ranking_manager:refresh_weekly_rankings(),
    ok.

%% 每周Boss刷新
weekly_boss_refresh() ->
    logger:log_info("执行每周Boss刷新"),
    %% 这里实现实际的每周Boss刷新逻辑
    %% 例如：boss_manager:refresh_weekly_bosses(),
    ok.

%% 每月刷新处理器示例

%% 每月排行榜刷新
monthly_ranking_refresh() ->
    logger:log_info("执行每月排行榜刷新"),
    %% 这里实现实际的每月排行榜刷新逻辑
    %% 例如：ranking_manager:refresh_monthly_rankings(),
    ok.

%% 每月VIP奖励刷新
monthly_vip_reward_refresh() ->
    logger:log_info("执行每月VIP奖励刷新"),
    %% 这里实现实际的每月VIP奖励刷新逻辑
    %% 例如：vip_manager:refresh_monthly_rewards(),
    ok.

%% 每月赛季刷新
monthly_season_refresh() ->
    logger:log_info("执行每月赛季刷新"),
    %% 这里实现实际的每月赛季刷新逻辑
    %% 例如：season_manager:refresh_monthly_season(),
    ok.

%% 注册所有刷新处理器
register_all_refresh() ->
    logger:log_info("注册所有时间刷新处理器"),
    
    %% 注册每日刷新处理器
    {ok, DailyQuestId} = time_refresh_manager:register_daily_refresh(?MODULE, daily_quest_refresh),
    {ok, DailyRewardId} = time_refresh_manager:register_daily_refresh(?MODULE, daily_reward_refresh),
    {ok, DailyActivityId} = time_refresh_manager:register_daily_refresh(?MODULE, daily_activity_refresh),
    
    %% 注册每周刷新处理器
    {ok, WeeklyQuestId} = time_refresh_manager:register_weekly_refresh(?MODULE, weekly_quest_refresh),
    {ok, WeeklyRankingId} = time_refresh_manager:register_weekly_refresh(?MODULE, weekly_ranking_refresh),
    {ok, WeeklyBossId} = time_refresh_manager:register_weekly_refresh(?MODULE, weekly_boss_refresh),
    
    %% 注册每月刷新处理器
    {ok, MonthlyRankingId} = time_refresh_manager:register_monthly_refresh(?MODULE, monthly_ranking_refresh),
    {ok, MonthlyVipRewardId} = time_refresh_manager:register_monthly_refresh(?MODULE, monthly_vip_reward_refresh),
    {ok, MonthlySeasonId} = time_refresh_manager:register_monthly_refresh(?MODULE, monthly_season_refresh),
    
    logger:log_info("刷新处理器注册完成"),
    
    %% 返回所有处理器ID
    #{
        daily => [DailyQuestId, DailyRewardId, DailyActivityId],
        weekly => [WeeklyQuestId, WeeklyRankingId, WeeklyBossId],
        monthly => [MonthlyRankingId, MonthlyVipRewardId, MonthlySeasonId]
    }. 