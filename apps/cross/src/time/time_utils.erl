-module(time_utils).
-export([
    %% 时间戳转换
    timestamp_to_datetime/1, datetime_to_timestamp/1,
    timestamp_to_date/1, date_to_timestamp/1,
    timestamp_to_time/1, time_to_timestamp/1,
    
    %% 时间日期解析
    parse_date/1, parse_time/1, parse_datetime/1,
    format_date/1, format_time/1, format_datetime/1,
    
    %% 时间判断
    is_same_day/2, is_same_week/2, is_same_month/2,
    is_same_year/2, is_today/1, is_this_week/1,
    is_this_month/1, is_this_year/1,
    
    %% 时间差计算
    time_diff/2, time_diff_days/2, time_diff_hours/2,
    time_diff_minutes/2, time_diff_seconds/2,
    
    %% 时间计算
    add_days/2, add_hours/2, add_minutes/2, add_seconds/2,
    subtract_days/2, subtract_hours/2, subtract_minutes/2, subtract_seconds/2,
    
    %% 获取时间信息
    get_current_timestamp/0, get_current_datetime/0,
    get_current_date/0, get_current_time/0,
    get_week_start/1, get_week_end/1,
    get_month_start/1, get_month_end/1,
    get_year_start/1, get_year_end/1,
    
    %% 服务器时间相关
    get_server_days/0, get_server_week/0, get_server_month/0,
    is_server_day_change/0, is_server_week_change/0, is_server_month_change/0
]).

%% 时间戳转换函数
timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp + 62167219200).

datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.

timestamp_to_date(Timestamp) ->
    {Date, _Time} = timestamp_to_datetime(Timestamp),
    Date.

date_to_timestamp(Date) ->
    datetime_to_timestamp({Date, {0, 0, 0}}).

timestamp_to_time(Timestamp) ->
    {_Date, Time} = timestamp_to_datetime(Timestamp),
    Time.

time_to_timestamp(Time) ->
    datetime_to_timestamp({{1970, 1, 1}, Time}).

%% 时间日期解析函数
parse_date(DateString) ->
    case string:split(DateString, "-") of
        [YearStr, MonthStr, DayStr] ->
            try
                Year = list_to_integer(YearStr),
                Month = list_to_integer(MonthStr),
                Day = list_to_integer(DayStr),
                {Year, Month, Day}
            catch
                _:_ -> {error, invalid_date_format}
            end;
        _ ->
            {error, invalid_date_format}
    end.

parse_time(TimeString) ->
    case string:split(TimeString, ":") of
        [HourStr, MinStr, SecStr] ->
            try
                Hour = list_to_integer(HourStr),
                Min = list_to_integer(MinStr),
                Sec = list_to_integer(SecStr),
                {Hour, Min, Sec}
            catch
                _:_ -> {error, invalid_time_format}
            end;
        _ ->
            {error, invalid_time_format}
    end.

parse_datetime(DateTimeString) ->
    case string:split(DateTimeString, " ") of
        [DateStr, TimeStr] ->
            case {parse_date(DateStr), parse_time(TimeStr)} of
                {{Year, Month, Day}, {Hour, Min, Sec}} ->
                    {{Year, Month, Day}, {Hour, Min, Sec}};
                _ ->
                    {error, invalid_datetime_format}
            end;
        _ ->
            {error, invalid_datetime_format}
    end.

%% 格式化函数
format_date({Year, Month, Day}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]).

format_time({Hour, Min, Sec}) ->
    io_lib:format("~2..0B:~2..0B:~2..0B", [Hour, Min, Sec]).

format_datetime({Date, Time}) ->
    DateStr = format_date(Date),
    TimeStr = format_time(Time),
    DateStr ++ " " ++ TimeStr.

%% 时间判断函数
is_same_day(DateTime1, DateTime2) ->
    {Date1, _} = DateTime1,
    {Date2, _} = DateTime2,
    Date1 =:= Date2.

is_same_week(DateTime1, DateTime2) ->
    WeekStart1 = get_week_start(DateTime1),
    WeekStart2 = get_week_start(DateTime2),
    WeekStart1 =:= WeekStart2.

is_same_month(DateTime1, DateTime2) ->
    {Date1, _} = DateTime1,
    {Date2, _} = DateTime2,
    {Year1, Month1, _} = Date1,
    {Year2, Month2, _} = Date2,
    Year1 =:= Year2 andalso Month1 =:= Month2.

is_same_year(DateTime1, DateTime2) ->
    {Date1, _} = DateTime1,
    {Date2, _} = DateTime2,
    {Year1, _, _} = Date1,
    {Year2, _, _} = Date2,
    Year1 =:= Year2.

is_today(DateTime) ->
    is_same_day(DateTime, get_current_datetime()).

is_this_week(DateTime) ->
    is_same_week(DateTime, get_current_datetime()).

is_this_month(DateTime) ->
    is_same_month(DateTime, get_current_datetime()).

is_this_year(DateTime) ->
    is_same_year(DateTime, get_current_datetime()).

%% 时间差计算函数
time_diff(DateTime1, DateTime2) ->
    Seconds1 = calendar:datetime_to_gregorian_seconds(DateTime1),
    Seconds2 = calendar:datetime_to_gregorian_seconds(DateTime2),
    abs(Seconds2 - Seconds1).

time_diff_days(DateTime1, DateTime2) ->
    Diff = time_diff(DateTime1, DateTime2),
    Diff div 86400.

time_diff_hours(DateTime1, DateTime2) ->
    Diff = time_diff(DateTime1, DateTime2),
    Diff div 3600.

time_diff_minutes(DateTime1, DateTime2) ->
    Diff = time_diff(DateTime1, DateTime2),
    Diff div 60.

time_diff_seconds(DateTime1, DateTime2) ->
    time_diff(DateTime1, DateTime2).

%% 时间计算函数
add_days(DateTime, Days) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds + (Days * 86400),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

add_hours(DateTime, Hours) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds + (Hours * 3600),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

add_minutes(DateTime, Minutes) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds + (Minutes * 60),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

add_seconds(DateTime, Seconds) ->
    TotalSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewTotalSeconds = TotalSeconds + Seconds,
    calendar:gregorian_seconds_to_datetime(NewTotalSeconds).

subtract_days(DateTime, Days) ->
    add_days(DateTime, -Days).

subtract_hours(DateTime, Hours) ->
    add_hours(DateTime, -Hours).

subtract_minutes(DateTime, Minutes) ->
    add_minutes(DateTime, -Minutes).

subtract_seconds(DateTime, Seconds) ->
    add_seconds(DateTime, -Seconds).

%% 获取时间信息函数
get_current_timestamp() ->
    erlang:system_time(second).

get_current_datetime() ->
    calendar:universal_time().

get_current_date() ->
    {Date, _} = get_current_datetime(),
    Date.

get_current_time() ->
    {_Date, Time} = get_current_datetime(),
    Time.

get_week_start(DateTime) ->
    {Date, _} = DateTime,
    DayOfWeek = calendar:day_of_the_week(Date),
    DaysToSubtract = DayOfWeek - 1,
    subtract_days(DateTime, DaysToSubtract).

get_week_end(DateTime) ->
    WeekStart = get_week_start(DateTime),
    add_days(WeekStart, 6).

get_month_start(DateTime) ->
    {Date, _} = DateTime,
    {Year, Month, _} = Date,
    {{Year, Month, 1}, {0, 0, 0}}.

get_month_end(DateTime) ->
    {Date, _} = DateTime,
    {Year, Month, _} = Date,
    DaysInMonth = calendar:last_day_of_the_month(Year, Month),
    {{Year, Month, DaysInMonth}, {23, 59, 59}}.

get_year_start(DateTime) ->
    {Date, _} = DateTime,
    {Year, _, _} = Date,
    {{Year, 1, 1}, {0, 0, 0}}.

get_year_end(DateTime) ->
    {Date, _} = DateTime,
    {Year, _, _} = Date,
    {{Year, 12, 31}, {23, 59, 59}}.

%% 服务器时间相关函数
get_server_days() ->
    case server_config:get_server_days() of
        {Days, _} -> Days;
        _ -> 0
    end.

get_server_week() ->
    Days = get_server_days(),
    (Days div 7) + 1.

get_server_month() ->
    Days = get_server_days(),
    (Days div 30) + 1.

is_server_day_change() ->
    % 这里需要实现与上次检查时间的比较逻辑
    % 暂时返回false，实际使用时需要记录上次检查时间
    false.

is_server_week_change() ->
    % 类似实现
    false.

is_server_month_change() ->
    % 类似实现
    false. 