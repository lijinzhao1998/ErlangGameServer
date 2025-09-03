%% RPC高级API接口模块
-module(rpc_api).

-export([
    %% 服务注册和发现
    register_service/2,
    unregister_service/1,
    discover_service/1,
    list_services/0,
    
    %% 同步调用
    call/3,
    call/4,
    
    %% 异步调用
    cast/3,
    
    %% 通知
    notify/2,
    
    %% 批量调用
    batch_call/2,
    batch_call/3,
    
    %% 连接管理
    connect/2,
    disconnect/0,
    is_connected/0,
    
    %% 监控和统计
    get_stats/0,
    get_health/0,
    
    %% 工具函数
    ping/1,
    echo/1
]).

%% 服务注册和发现

%% 注册当前节点上的服务
register_service(ServiceName, ServicePid) ->
    Node = node(),
    rpc_registry:register_service(ServiceName, Node, ServicePid).

%% 注销服务
unregister_service(ServiceName) ->
    Node = node(),
    rpc_registry:unregister_service(ServiceName, Node).

%% 发现服务
discover_service(ServiceName) ->
    rpc_registry:get_service(ServiceName).

%% 列出所有服务
list_services() ->
    rpc_registry:list_services().

%% 同步调用远程服务
call(Service, Method, Params) ->
    call(Service, Method, Params, 5000).

call(Service, Method, Params, Timeout) ->
    case discover_service(Service) of
        {ok, ServiceInfo} ->
            % 这里应该根据服务信息选择最佳节点进行调用
            % 暂时使用简单的节点选择
            Node = ServiceInfo#service_info.node,
            rpc:call(Node, Service, Method, [Params], Timeout);
        {error, Reason} ->
            {error, {service_not_found, Reason}}
    end.

%% 异步调用远程服务
cast(Service, Method, Params) ->
    case discover_service(Service) of
        {ok, ServiceInfo} ->
            Node = ServiceInfo#service_info.node,
            rpc:cast(Node, Service, Method, [Params]);
        {error, Reason} ->
            {error, {service_not_found, Reason}}
    end.

%% 发送通知
notify(Service, Method) ->
    case discover_service(Service) of
        {ok, ServiceInfo} ->
            Node = ServiceInfo#service_info.node,
            rpc:cast(Node, Service, Method, []);
        {error, Reason} ->
            {error, {service_not_found, Reason}}
    end.

%% 批量调用
batch_call(Calls, Timeout) ->
    batch_call(Calls, Timeout, []).

batch_call([], _Timeout, Results) ->
    lists:reverse(Results);
batch_call([{Service, Method, Params} | Rest], Timeout, Results) ->
    case call(Service, Method, Params, Timeout) of
        {ok, Result} ->
            batch_call(Rest, Timeout, [{ok, Result} | Results]);
        {error, Reason} ->
            batch_call(Rest, Timeout, [{error, Reason} | Results])
    end.

%% 连接管理

%% 连接到RPC服务器
connect(Address, Port) ->
    case rpc_client:start_link(Address, Port) of
        {ok, _Pid} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% 断开连接
disconnect() ->
    rpc_client:disconnect().

%% 检查是否已连接
is_connected() ->
    case whereis(rpc_client) of
        undefined -> false;
        _ -> true
    end.

%% 监控和统计

%% 获取连接统计
get_stats() ->
    rpc_monitor:get_connection_stats().

%% 获取服务健康状态
get_health() ->
    rpc_monitor:get_service_health().

%% 工具函数

%% Ping服务
ping(Service) ->
    call(Service, ping, [], 1000).

%% Echo测试
echo(Message) ->
    call(echo_service, echo, Message, 1000).

%% 内部函数：获取服务信息记录字段
-record(service_info, {
    name,           % 服务名称
    node,           % 节点名称
    pid,            % 进程ID
    address,        % 服务地址
    port,           % 服务端口
    status,         % 服务状态
    last_heartbeat, % 最后心跳时间
    metadata        % 服务元数据
}). 