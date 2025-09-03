-module(time_refresh_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([register_daily_refresh/2, register_weekly_refresh/2, register_monthly_refresh/2]).
-export([unregister_refresh/2, get_refresh_handlers/1]).
-export([force_refresh/1, check_refresh/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(REFRESH_CHECK_INTERVAL, 60000). % 每分钟检查一次

-record(refresh_handler, {
    id :: binary(),
    type :: daily | weekly | monthly,
    module :: atom(),
    function :: atom(),
    args :: list(),
    last_refresh :: integer(),
    is_active :: boolean()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% 注册每日刷新处理器
register_daily_refresh(Module, Function) ->
    register_daily_refresh(Module, Function, []).

register_daily_refresh(Module, Function, Args) ->
    gen_server:call(?MODULE, {register_refresh, daily, Module, Function, Args}).

%% 注册每周刷新处理器
register_weekly_refresh(Module, Function) ->
    register_weekly_refresh(Module, Function, []).

register_weekly_refresh(Module, Function, Args) ->
    gen_server:call(?MODULE, {register_refresh, weekly, Module, Function, Args}).

%% 注册每月刷新处理器
register_monthly_refresh(Module, Function) ->
    register_monthly_refresh(Module, Function, []).

register_monthly_refresh(Module, Function, Args) ->
    gen_server:call(?MODULE, {register_refresh, monthly, Module, Function, Args}).

%% 注销刷新处理器
unregister_refresh(Type, Id) ->
    gen_server:call(?MODULE, {unregister_refresh, Type, Id}).

%% 获取指定类型的刷新处理器
get_refresh_handlers(Type) ->
    gen_server:call(?MODULE, {get_refresh_handlers, Type}).

%% 强制刷新指定类型
force_refresh(Type) ->
    gen_server:call(?MODULE, {force_refresh, Type}).

%% 手动检查刷新
check_refresh() ->
    gen_server:cast(?MODULE, check_refresh).

%% gen_server callbacks
init([]) ->
    %% 启动定时检查进程
    erlang:send_after(?REFRESH_CHECK_INTERVAL, self(), check_refresh),
    {ok, #{
        daily_handlers => #{},
        weekly_handlers => #{},
        monthly_handlers => #{},
        last_daily_refresh => 0,
        last_weekly_refresh => 0,
        last_monthly_refresh => 0
    }}.

handle_call({register_refresh, Type, Module, Function, Args}, _From, State) ->
    Id = generate_handler_id(),
    Handler = #refresh_handler{
        id = Id,
        type = Type,
        module = Module,
        function = Function,
        args = Args,
        last_refresh = 0,
        is_active = true
    },
    
    NewState = case Type of
        daily ->
            Handlers = maps:get(daily_handlers, State, #{}),
            State#{daily_handlers => Handlers#{Id => Handler}};
        weekly ->
            Handlers = maps:get(weekly_handlers, State, #{}),
            State#{weekly_handlers => Handlers#{Id => Handler}};
        monthly ->
            Handlers = maps:get(monthly_handlers, State, #{}),
            State#{monthly_handlers => Handlers#{Id => Handler}}
    end,
    
    logger:log_info("注册~p刷新处理器 ~s: ~p:~p/~p", 
                   [Type, Id, Module, Function, length(Args)]),
    
    {reply, {ok, Id}, NewState};

handle_call({unregister_refresh, Type, Id}, _From, State) ->
    NewState = case Type of
        daily ->
            Handlers = maps:get(daily_handlers, State, #{}),
            State#{daily_handlers => maps:remove(Id, Handlers)};
        weekly ->
            Handlers = maps:get(weekly_handlers, State, #{}),
            State#{weekly_handlers => maps:remove(Id, Handlers)};
        monthly ->
            Handlers = maps:get(monthly_handlers, State, #{}),
            State#{monthly_handlers => maps:remove(Id, Handlers)}
    end,
    
    logger:log_info("注销~p刷新处理器 ~s", [Type, Id]),
    {reply, ok, NewState};

handle_call({get_refresh_handlers, Type}, _From, State) ->
    Handlers = case Type of
        daily -> maps:get(daily_handlers, State, #{});
        weekly -> maps:get(weekly_handlers, State, #{});
        monthly -> maps:get(monthly_handlers, State, #{})
    end,
    
    HandlerList = maps:fold(fun(Id, Handler, Acc) ->
        [{Id, handler_to_map(Handler)} | Acc]
    end, [], Handlers),
    
    {reply, {ok, HandlerList}, State};

handle_call({force_refresh, Type}, _From, State) ->
    {Result, NewState} = execute_refresh(Type, State),
    {reply, Result, NewState};

handle_call(stop, _From, State) ->
    logger:log_info("TimeRefreshManager停止"),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(check_refresh, State) ->
    {NewState, _} = check_all_refresh(State),
    %% 重新启动定时检查
    erlang:send_after(?REFRESH_CHECK_INTERVAL, self(), check_refresh),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_refresh, State) ->
    {NewState, _} = check_all_refresh(State),
    %% 重新启动定时检查
    erlang:send_after(?REFRESH_CHECK_INTERVAL, self(), check_refresh),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数
generate_handler_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:system_time(microsecond),
    Id = (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs,
    list_to_binary(integer_to_list(Id)).

check_all_refresh(State) ->
    {State1, DailyChanged} = check_daily_refresh(State),
    {State2, WeeklyChanged} = check_weekly_refresh(State1),
    {State3, MonthlyChanged} = check_monthly_refresh(State2),
    
    if
        DailyChanged orelse WeeklyChanged orelse MonthlyChanged ->
            logger:log_info("检测到时间变化: 日变化=~p, 周变化=~p, 月变化=~p", 
                           [DailyChanged, WeeklyChanged, MonthlyChanged]);
        true ->
            ok
    end,
    
    {State3, {DailyChanged, WeeklyChanged, MonthlyChanged}}.

check_daily_refresh(State) ->
    Now = time_utils:get_current_date(),
    LastRefresh = maps:get(last_daily_refresh, State, 0),
    
    case is_different_day(Now, LastRefresh) of
        true ->
            logger:log_info("检测到日变化，执行每日刷新"),
            {Result, NewState} = execute_refresh(daily, State),
            case Result of
                ok ->
                    {NewState#{last_daily_refresh => Now}, true};
                {error, Reason} ->
                    logger:log_error("每日刷新执行失败: ~p", [Reason]),
                    {NewState#{last_daily_refresh => Now}, true}
            end;
        false ->
            {State, false}
    end.

check_weekly_refresh(State) ->
    Now = time_utils:get_current_date(),
    LastRefresh = maps:get(last_weekly_refresh, State, 0),
    
    case is_different_week(Now, LastRefresh) of
        true ->
            logger:log_info("检测到周变化，执行每周刷新"),
            {Result, NewState} = execute_refresh(weekly, State),
            case Result of
                ok ->
                    {NewState#{last_weekly_refresh => Now}, true};
                {error, Reason} ->
                    logger:log_error("每周刷新执行失败: ~p", [Reason]),
                    {NewState#{last_weekly_refresh => Now}, true}
            end;
        false ->
            {State, false}
    end.

check_monthly_refresh(State) ->
    Now = time_utils:get_current_date(),
    LastRefresh = maps:get(last_monthly_refresh, State, 0),
    
    case is_different_month(Now, LastRefresh) of
        true ->
            logger:log_info("检测到月变化，执行每月刷新"),
            {Result, NewState} = execute_refresh(monthly, State),
            case Result of
                ok ->
                    {NewState#{last_monthly_refresh => Now}, true};
                {error, Reason} ->
                    logger:log_error("每月刷新执行失败: ~p", [Reason]),
                    {NewState#{last_monthly_refresh => Now}, true}
            end;
        false ->
            {State, false}
    end.

execute_refresh(Type, State) ->
    Handlers = case Type of
        daily -> maps:get(daily_handlers, State, #{});
        weekly -> maps:get(weekly_handlers, State, #{});
        monthly -> maps:get(monthly_handlers, State, #{})
    end,
    
    case maps:size(Handlers) of
        0 ->
            {ok, State};
        _ ->
            execute_handlers(Type, Handlers, State)
    end.

execute_handlers(Type, Handlers, State) ->
    Results = maps:fold(fun(Id, Handler, Acc) ->
        case execute_single_handler(Handler) of
            ok ->
                [{Id, ok} | Acc];
            {error, Reason} ->
                [{Id, {error, Reason}} | Acc]
        end
    end, [], Handlers),
    
    %% 记录执行结果
    lists:foreach(fun({Id, Result}) ->
        case Result of
            ok ->
                logger:log_info("~p刷新处理器 ~s 执行成功", [Type, Id]);
            {error, Reason} ->
                logger:log_error("~p刷新处理器 ~s 执行失败: ~p", [Type, Id, Reason])
        end
    end, Results),
    
    {ok, State}.

execute_single_handler(Handler) ->
    try
        Module = Handler#refresh_handler.module,
        Function = Handler#refresh_handler.function,
        Args = Handler#refresh_handler.args,
        
        case erlang:function_exported(Module, Function, length(Args)) of
            true ->
                apply(Module, Function, Args),
                ok;
            false ->
                {error, function_not_exported}
        end
    catch
        Class:Reason:Stacktrace ->
            logger:log_error("刷新处理器执行异常: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {exception, Class, Reason}}
    end.

is_different_day(Date1, Date2) ->
    Date1 =/= Date2.

is_different_week(Date1, Date2) ->
    WeekStart1 = time_utils:get_week_start({Date1, {0, 0, 0}}),
    WeekStart2 = time_utils:get_week_start({Date2, {0, 0, 0}}),
    {Date1, _} = WeekStart1,
    {Date2, _} = WeekStart2,
    Date1 =/= Date2.

is_different_month(Date1, Date2) ->
    {Year1, Month1, _} = Date1,
    {Year2, Month2, _} = Date2,
    Year1 =/= Year2 orelse Month1 =/= Month2.

handler_to_map(Handler) ->
    #{
        id => Handler#refresh_handler.id,
        type => Handler#refresh_handler.type,
        module => Handler#refresh_handler.module,
        function => Handler#refresh_handler.function,
        args => Handler#refresh_handler.args,
        last_refresh => Handler#refresh_handler.last_refresh,
        is_active => Handler#refresh_handler.is_active
    }. 