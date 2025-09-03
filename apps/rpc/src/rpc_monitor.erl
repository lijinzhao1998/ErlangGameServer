%% RPC监控模块
-module(rpc_monitor).
-behaviour(gen_server).

-export([
    start_link/0,
    add_connection/2,
    remove_connection/1,
    get_connection_stats/0,
    get_service_health/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(connection_info, {
    id,
    remote_address,
    remote_port,
    local_port,
    connected_at,
    last_activity,
    message_count,
    status
}).

-record(state, {
    connections = #{},
    connection_count = 0,
    total_messages = 0,
    monitor_timer
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_connection(Id, ConnectionInfo) ->
    gen_server:cast(?MODULE, {add_connection, Id, ConnectionInfo}).

remove_connection(Id) ->
    gen_server:cast(?MODULE, {remove_connection, Id}).

get_connection_stats() ->
    gen_server:call(?MODULE, get_connection_stats).

get_service_health() ->
    gen_server:call(?MODULE, get_service_health).

init([]) ->
    % 启动监控定时器
    Timer = erlang:send_after(60000, self(), monitor_tick),
    {ok, #state{monitor_timer = Timer}}.

handle_call(get_connection_stats, _From, State) ->
    Stats = #{
        total_connections => State#state.connection_count,
        active_connections => maps:size(State#state.connections),
        total_messages => State#state.total_messages
    },
    {reply, Stats, State};

handle_call(get_service_health, _From, State) ->
    Health = collect_service_health(State),
    {reply, Health, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast({add_connection, Id, ConnectionInfo}, State) ->
    Connections = State#state.connections#{Id => ConnectionInfo},
    ConnectionCount = State#state.connection_count + 1,
    {noreply, State#state{connections = Connections, connection_count = ConnectionCount}};

handle_cast({remove_connection, Id}, State) ->
    Connections = maps:remove(Id, State#state.connections),
    {noreply, State#state{connections = Connections}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitor_tick, State) ->
    % 执行监控任务
    monitor_connections(State),
    
    % 重新启动定时器
    Timer = erlang:send_after(60000, self(), monitor_tick),
    {noreply, State#state{monitor_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 监控连接
monitor_connections(State) ->
    Now = erlang:system_time(millisecond),
    Connections = State#state.connections,
    
    % 检查连接超时
    ActiveConnections = maps:filter(fun(_Id, ConnInfo) ->
        Timeout = 300000, % 5分钟超时
        (Now - ConnInfo#connection_info.last_activity) < Timeout
    end, Connections),
    
    % 更新连接状态
    UpdatedConnections = maps:map(fun(_Id, ConnInfo) ->
        ConnInfo#connection_info{status = active}
    end, ActiveConnections),
    
    % 记录监控信息
    io:format("RPC Monitor: ~p active connections, ~p total messages~n", 
              [maps:size(UpdatedConnections), State#state.total_messages]).

%% 收集服务健康状态
collect_service_health(State) ->
    #{
        connections => maps:size(State#state.connections),
        total_connections => State#state.connection_count,
        total_messages => State#state.total_messages,
        uptime => get_uptime(),
        memory_usage => get_memory_usage(),
        node_status => node()
    }.

%% 获取运行时间
get_uptime() ->
    case statistics(wall_clock) of
        {_, Uptime} -> Uptime;
        _ -> 0
    end.

%% 获取内存使用
get_memory_usage() ->
    case erlang:memory() of
        Memory when is_list(Memory) ->
            case lists:keyfind(total, 1, Memory) of
                {total, Total} -> Total;
                _ -> 0
            end;
        _ -> 0
    end. 