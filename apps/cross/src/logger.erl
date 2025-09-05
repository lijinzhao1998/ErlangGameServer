-module(logger).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([log/3, log/4, log_info/1, log_warning/1, log_error/1, log_debug/1]).
-export([log_with_stack/4, log_with_stack/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOG_DIR, "logs").
-define(LOG_TYPES, [info, warning, error, debug]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% 日志接口
log(Type, Module, Message) ->
    log(Type, Module, Message, []).

log(Type, Module, Message, Args) ->
    gen_server:cast(?MODULE, {log, Type, Module, Message, Args, undefined}).

%% 带堆栈跟踪的日志
log_with_stack(Type, Module, Message, StackTrace) ->
    log_with_stack(Type, Module, Message, [], StackTrace).

log_with_stack(Type, Module, Message, Args, StackTrace) ->
    gen_server:cast(?MODULE, {log, Type, Module, Message, Args, StackTrace}).

log_info(Message) -> log(info, ?MODULE, Message).
log_warning(Message) -> log(warning, ?MODULE, Message).
log_error(Message) -> log(error, ?MODULE, Message).
log_debug(Message) -> log(debug, ?MODULE, Message).

%% gen_server callbacks
init([]) ->
    %% 创建日志目录
    file:make_dir(?LOG_DIR),
    lists:foreach(fun(Type) -> 
        file:make_dir(filename:join(?LOG_DIR, atom_to_list(Type)))
    end, ?LOG_TYPES),
    {ok, #{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Type, Module, Message, Args, StackTrace}, State) ->
    do_log(Type, Module, Message, Args, StackTrace),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数
do_log(Type, Module, Message, Args, StackTrace) ->
    %% 获取当前进程信息
    ProcessInfo = get_process_info(),
    
    %% 获取调用栈信息
    CallStack = get_call_stack(),
    
    %% 格式化消息
    FormattedMessage = case Args of
        [] -> Message;
        _ -> io_lib:format(Message, Args)
    end,
    
    %% 构建Debugger Error Log格式的日志行
    LogLine = format_debugger_log(Type, Module, FormattedMessage, ProcessInfo, CallStack, StackTrace),
    
    %% 同时输出到控制台和文件
    io:format("~s", [LogLine]),
    
    %% 写入对应类型的日志文件
    DateStr = get_date_string(),
    LogFile = filename:join([?LOG_DIR, atom_to_list(Type), DateStr ++ ".log"]),
    file:write_file(LogFile, LogLine, [append]).

%% 获取进程信息
get_process_info() ->
    try
        Pid = self(),
        Dict = case process_info(Pid, dictionary) of
            {dictionary, D} -> D;
            _ -> []
        end,
        Name = case process_info(Pid, registered_name) of
            {registered_name, N} -> N;
            _ -> undefined
        end,
        #{pid => Pid, name => Name, dictionary => Dict}
    catch
        _:_ -> #{pid => self(), name => undefined, dictionary => []}
    end.

%% 获取调用栈信息
get_call_stack() ->
    try
        {current_stacktrace, Stack} = process_info(self(), current_stacktrace),
        Stack
    catch
        _:_ -> []
    end.

%% 格式化Debugger Error Log
format_debugger_log(Type, Module, Message, ProcessInfo, CallStack, StackTrace) ->
    %% 获取时间戳
    Timestamp = erlang:system_time(millisecond),
    Date = calendar:system_time_to_universal_time(Timestamp, second),
    DateStr = format_date(Date),
    TimeStr = format_time(Date),
    
    %% 获取进程信息
    Pid = maps:get(pid, ProcessInfo),
    ProcessName = maps:get(name, ProcessInfo),
    
    %% 获取调用位置信息
    CallLocation = get_call_location(CallStack),
    
    %% 构建日志头部
    Header = io_lib:format("=ERROR REPORT==== ~s ~s ===~n", [DateStr, TimeStr]),
    
    %% 构建进程信息
    ProcessInfoStr = io_lib:format("Process: ~p (~s)~n", [Pid, ProcessName]),
    
    %% 构建模块和位置信息
    LocationStr = io_lib:format("Module: ~p~n", [Module]),
    
    %% 构建日志级别
    LevelStr = io_lib:format("Level: ~p~n", [Type]),
    
    %% 构建消息
    MessageStr = io_lib:format("Message: ~s~n", [Message]),
    
    %% 构建调用位置
    CallLocationStr = case CallLocation of
        {File, Line} -> io_lib:format("Location: ~s:~p~n", [File, Line]);
        _ -> ""
    end,
    
    %% 构建调用栈（如果有）
    CallStackStr = case CallStack of
        [] -> "";
        _ -> format_call_stack(CallStack)
    end,
    
    %% 构建堆栈跟踪（如果有）
    StackTraceStr = case StackTrace of
        undefined -> "";
        _ -> format_stack_trace(StackTrace)
    end,
    
    %% 构建分隔线
    Separator = "~n",
    
    %% 组合所有部分
    io_lib:format("~s~s~s~s~s~s~s~s~s", [
        Header,
        ProcessInfoStr,
        LocationStr,
        LevelStr,
        MessageStr,
        CallLocationStr,
        CallStackStr,
        StackTraceStr,
        Separator
    ]).

%% 获取调用位置
get_call_location([]) ->
    {unknown, 0};
get_call_location([{Module, _Function, _Arity, Location} | _]) ->
    case Location of
        [{file, File}, {line, Line}] -> {File, Line};
        _ -> {atom_to_list(Module), 0}
    end;
get_call_location(_) ->
    {unknown, 0}.

%% 格式化调用栈
format_call_stack([]) ->
    "";
format_call_stack(CallStack) ->
    StackStr = lists:foldl(fun({Module, Function, Arity, Location}, Acc) ->
        LocationStr = case Location of
            [{file, File}, {line, Line}] -> io_lib:format("~s:~p", [File, Line]);
            _ -> "unknown"
        end,
        Acc ++ io_lib:format("  ~p:~p/~p at ~s~n", [Module, Function, Arity, LocationStr])
    end, "Call Stack:~n", CallStack),
    StackStr.

%% 格式化堆栈跟踪
format_stack_trace(StackTrace) ->
    case StackTrace of
        [] -> "";
        _ ->
            TraceStr = lists:foldl(fun({Module, Function, Arity, Location}, Acc) ->
                LocationStr = case Location of
                    [{file, File}, {line, Line}] -> io_lib:format("~s:~p", [File, Line]);
                    _ -> "unknown"
                end,
                Acc ++ io_lib:format("  ~p:~p/~p at ~s~n", [Module, Function, Arity, LocationStr])
            end, "Stack Trace:~n", StackTrace),
            TraceStr
    end.

%% 获取日期字符串
get_date_string() ->
    Timestamp = erlang:system_time(millisecond),
    Date = calendar:system_time_to_universal_time(Timestamp, second),
    format_date(Date).

format_date({{Year, Month, Day}, _Time}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]).

format_time({_Date, {Hour, Min, Sec}}) ->
    io_lib:format("~2..0B:~2..0B:~2..0B", [Hour, Min, Sec]). 