-module(role_timer).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([start_timer/5, stop_timer/2, get_timers/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MIN_INTERVAL, 1000). % 最小间隔1秒
-define(MAX_INTERVAL, 86400000). % 最大间隔24小时

-record(timer_info, {
    id :: binary(),
    role_id :: integer(),
    module :: atom(),
    function :: atom(),
    args :: list(),
    interval :: integer(),
    next_execute :: integer(),
    repeat_count :: integer(), % -1表示无限重复
    is_active :: boolean()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% 启动定时器
%% start_timer(RoleId, Module, Function, Args, Interval, RepeatCount)
%% start_timer(RoleId, Module, Function, Args, Interval) -> 默认无限重复
start_timer(RoleId, Module, Function, Args, Interval, RepeatCount) ->
    gen_server:call(?MODULE, {start_timer, RoleId, Module, Function, Args, Interval, RepeatCount}).

start_timer(RoleId, Module, Function, Args, Interval) ->
    start_timer(RoleId, Module, Function, Args, Interval, -1).

%% 停止定时器
stop_timer(RoleId, TimerId) ->
    gen_server:call(?MODULE, {stop_timer, RoleId, TimerId}).

%% 获取玩家的所有定时器
get_timers(RoleId) ->
    gen_server:call(?MODULE, {get_timers, RoleId}).

%% gen_server callbacks
init([]) ->
    %% 启动定时检查进程
    erlang:send_after(1000, self(), check_timers),
    {ok, #{timers => #{}, next_check => 1000}}.

handle_call({start_timer, RoleId, Module, Function, Args, Interval, RepeatCount}, _From, State) ->
    case validate_timer_params(Interval, RepeatCount) of
        ok ->
            TimerId = generate_timer_id(),
            Now = erlang:system_time(millisecond),
            NextExecute = Now + Interval,
            
            TimerInfo = #timer_info{
                id = TimerId,
                role_id = RoleId,
                module = Module,
                function = Function,
                args = Args,
                interval = Interval,
                next_execute = NextExecute,
                repeat_count = RepeatCount,
                is_active = true
            },
            
            Timers = maps:get(timers, State, #{}),
            RoleTimers = maps:get(RoleId, Timers, #{}),
            NewRoleTimers = RoleTimers#{TimerId => TimerInfo},
            NewTimers = Timers#{RoleId => NewRoleTimers},
            
            logger:log_info("玩家 ~p 启动定时器 ~s: ~p:~p/~p 间隔 ~pms", 
                          [RoleId, TimerId, Module, Function, length(Args), Interval]),
            
            {reply, {ok, TimerId}, State#{timers => NewTimers}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({stop_timer, RoleId, TimerId}, _From, State) ->
    Timers = maps:get(timers, State, #{}),
    RoleTimers = maps:get(RoleId, Timers, #{}),
    
    case maps:find(TimerId, RoleTimers) of
        {ok, _TimerInfo} ->
            NewRoleTimers = maps:remove(TimerId, RoleTimers),
            NewTimers = case maps:size(NewRoleTimers) of
                0 -> maps:remove(RoleId, Timers);
                _ -> Timers#{RoleId => NewRoleTimers}
            end,
            
            logger:log_info("玩家 ~p 停止定时器 ~s", [RoleId, TimerId]),
            {reply, {ok, stopped}, State#{timers => NewTimers}};
        error ->
            {reply, {error, timer_not_found}, State}
    end;

handle_call({get_timers, RoleId}, _From, State) ->
    Timers = maps:get(timers, State, #{}),
    RoleTimers = maps:get(RoleId, Timers, #{}),
    
    TimerList = maps:fold(fun(TimerId, TimerInfo, Acc) ->
        [{TimerId, timer_info_to_map(TimerInfo)} | Acc]
    end, [], RoleTimers),
    
    {reply, {ok, TimerList}, State};

handle_call(stop, _From, State) ->
    logger:log_info("RoleTimer停止，清理所有定时器"),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_timers, State) ->
    Timers = maps:get(timers, State, #{}),
    Now = erlang:system_time(millisecond),
    
    {NewTimers, NextCheck} = maps:fold(fun(RoleId, RoleTimers, {AccTimers, MinNext}) ->
        {NewRoleTimers, RoleMinNext} = process_role_timers(RoleId, RoleTimers, Now),
        
        case maps:size(NewRoleTimers) of
            0 -> {AccTimers, MinNext}; % 该玩家没有定时器了
            _ -> {AccTimers#{RoleId => NewRoleTimers}, min(MinNext, RoleMinNext)}
        end
    end, {#{}, ?MAX_INTERVAL}, Timers),
    
    %% 重新启动定时检查
    erlang:send_after(NextCheck, self(), check_timers),
    {noreply, State#{timers => NewTimers, next_check => NextCheck}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数
validate_timer_params(Interval, RepeatCount) ->
    case Interval >= ?MIN_INTERVAL andalso Interval =< ?MAX_INTERVAL of
        false ->
            {error, invalid_interval};
        true ->
            case RepeatCount >= -1 of
                false ->
                    {error, invalid_repeat_count};
                true ->
                    ok
            end
    end.

generate_timer_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:system_time(microsecond),
    Id = (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs,
    list_to_binary(integer_to_list(Id)).

process_role_timers(RoleId, RoleTimers, Now) ->
    maps:fold(fun(TimerId, TimerInfo, {AccTimers, MinNext}) ->
        case process_single_timer(RoleId, TimerInfo, Now) of
            {ok, NewTimerInfo} ->
                NextExecute = NewTimerInfo#timer_info.next_execute,
                {AccTimers#{TimerId => NewTimerInfo}, min(MinNext, NextExecute - Now)};
            {stop, _} ->
                {AccTimers, MinNext};
            {error, Reason} ->
                logger:log_error("处理定时器 ~s 失败: ~p", [TimerId, Reason]),
                {AccTimers, MinNext}
        end
    end, {#{}, ?MAX_INTERVAL}, RoleTimers).

process_single_timer(_RoleId, TimerInfo, Now) ->
    case TimerInfo#timer_info.next_execute =< Now of
        false ->
            {ok, TimerInfo};
        true ->
            %% 执行定时器回调
            case execute_timer_callback(TimerInfo) of
                ok ->
                    %% 计算下次执行时间
                    case calculate_next_execution(TimerInfo) of
                        {ok, NextExecute, NewRepeatCount} ->
                            if
                                NewRepeatCount == 0 ->
                                    logger:log_info("定时器 ~s 执行完成，停止", [TimerInfo#timer_info.id]),
                                    {stop, completed};
                                true ->
                                    NewTimerInfo = TimerInfo#timer_info{
                                        next_execute = NextExecute,
                                        repeat_count = NewRepeatCount
                                    },
                                    {ok, NewTimerInfo}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    logger:log_error("执行定时器回调失败: ~p", [Reason]),
                    {error, Reason}
            end
    end.

execute_timer_callback(TimerInfo) ->
    try
        Module = TimerInfo#timer_info.module,
        Function = TimerInfo#timer_info.function,
        Args = TimerInfo#timer_info.args,
        RoleId = TimerInfo#timer_info.role_id,
        
        %% 添加RoleId作为第一个参数
        FullArgs = [RoleId | Args],
        
        case erlang:function_exported(Module, Function, length(FullArgs)) of
            true ->
                apply(Module, Function, FullArgs),
                ok;
            false ->
                {error, function_not_exported}
        end
    catch
        Class:Reason:Stacktrace ->
            logger:log_error("定时器回调执行异常: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {exception, Class, Reason}}
    end.

calculate_next_execution(TimerInfo) ->
    try
        NextExecute = TimerInfo#timer_info.next_execute + TimerInfo#timer_info.interval,
        RepeatCount = TimerInfo#timer_info.repeat_count,
        
        if
            RepeatCount == -1 ->
                {ok, NextExecute, -1}; % 无限重复
            RepeatCount > 0 ->
                {ok, NextExecute, RepeatCount - 1};
            true ->
                {error, invalid_repeat_count}
        end
    catch
        _:_ ->
            {error, calculation_failed}
    end.

timer_info_to_map(TimerInfo) ->
    #{
        id => TimerInfo#timer_info.id,
        role_id => TimerInfo#timer_info.role_id,
        module => TimerInfo#timer_info.module,
        function => TimerInfo#timer_info.function,
        args => TimerInfo#timer_info.args,
        interval => TimerInfo#timer_info.interval,
        next_execute => TimerInfo#timer_info.next_execute,
        repeat_count => TimerInfo#timer_info.repeat_count,
        is_active => TimerInfo#timer_info.is_active
    }. 