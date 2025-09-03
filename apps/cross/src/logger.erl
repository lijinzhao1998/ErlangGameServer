-module(logger).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([log/3, log/4, log_info/1, log_warning/1, log_error/1, log_debug/1]).
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
    gen_server:cast(?MODULE, {log, Type, Module, Message, Args}).

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

handle_cast({log, Type, Module, Message, Args}, State) ->
    do_log(Type, Module, Message, Args),
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
do_log(Type, Module, Message, Args) ->
    Timestamp = erlang:system_time(millisecond),
    Date = calendar:system_time_to_universal_time(Timestamp, second),
    DateStr = format_date(Date),
    TimeStr = format_time(Date),
    
    %% 格式化消息
    FormattedMessage = case Args of
        [] -> Message;
        _ -> io_lib:format(Message, Args)
    end,
    
    %% 构建日志行
    LogLine = io_lib:format("[~s ~s] [~p] ~s~n", 
                           [DateStr, TimeStr, Module, FormattedMessage]),
    
    %% 写入对应类型的日志文件
    LogFile = filename:join([?LOG_DIR, atom_to_list(Type), DateStr ++ ".log"]),
    file:write_file(LogFile, LogLine, [append]).

format_date({{Year, Month, Day}, _Time}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]).

format_time({_Date, {Hour, Min, Sec}}) ->
    io_lib:format("~2..0B:~2..0B:~2..0B", [Hour, Min, Sec]). 