%% RPC服务注册表
-module(rpc_registry).
-behaviour(gen_server).

-export([
    start_link/0,
    register_service/3,
    unregister_service/2,
    get_service/1,
    list_services/0,
    update_service_status/3
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(service_info, {
    name,           % 服务名称
    node,           % 节点名称
    pid,            % 进程ID
    address,        % 服务地址
    port,           % 服务端口
    status,         % 服务状态 (online/offline/maintenance)
    last_heartbeat, % 最后心跳时间
    metadata        % 服务元数据
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 注册服务
register_service(Name, Node, Pid) ->
    gen_server:call(?MODULE, {register_service, Name, Node, Pid}).

%% 注销服务
unregister_service(Name, Node) ->
    gen_server:call(?MODULE, {unregister_service, Name, Node}).

%% 获取服务信息
get_service(Name) ->
    gen_server:call(?MODULE, {get_service, Name}).

%% 列出所有服务
list_services() ->
    gen_server:call(?MODULE, list_services).

%% 更新服务状态
update_service_status(Name, Node, Status) ->
    gen_server:call(?MODULE, {update_service_status, Name, Node, Status}).

init([]) ->
    % 创建ETS表存储服务信息
    ets:new(services, [set, public, named_table]),
    {ok, #{}}.

handle_call({register_service, Name, Node, Pid}, _From, State) ->
    ServiceInfo = #service_info{
        name = Name,
        node = Node,
        pid = Pid,
        address = get_node_address(Node),
        port = get_node_port(Node),
        status = online,
        last_heartbeat = erlang:system_time(millisecond),
        metadata = #{}
    },
    
    % 存储服务信息
    ets:insert(services, {Name, ServiceInfo}),
    
    io:format("Service registered: ~s on ~s (~p)~n", [Name, Node, Pid]),
    {reply, {ok, ServiceInfo}, State};

handle_call({unregister_service, Name, Node}, _From, State) ->
    case ets:lookup(services, Name) of
        [{Name, #service_info{node = Node} = ServiceInfo}] ->
            ets:delete(services, Name),
            io:format("Service unregistered: ~s on ~s~n", [Name, Node]),
            {reply, {ok, ServiceInfo}, State};
        _ ->
            {reply, {error, service_not_found}, State}
    end;

handle_call({get_service, Name}, _From, State) ->
    case ets:lookup(services, Name) of
        [{Name, ServiceInfo}] ->
            {reply, {ok, ServiceInfo}, State};
        [] ->
            {reply, {error, service_not_found}, State}
    end;

handle_call(list_services, _From, State) ->
    Services = ets:tab2list(services),
    {reply, {ok, Services}, State};

handle_call({update_service_status, Name, Node, Status}, _From, State) ->
    case ets:lookup(services, Name) of
        [{Name, ServiceInfo}] when ServiceInfo#service_info.node =:= Node ->
            UpdatedService = ServiceInfo#service_info{status = Status},
            ets:insert(services, {Name, UpdatedService}),
            {reply, {ok, UpdatedService}, State};
        _ ->
            {reply, {error, service_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数：获取节点地址
get_node_address(Node) ->
    case net_adm:names() of
        {ok, Names} ->
            case lists:keyfind(atom_to_list(Node), 1, Names) of
                {_, Address} -> Address;
                false -> "localhost"
            end;
        _ ->
            "localhost"
    end.

%% 内部函数：获取节点端口
get_node_port(_Node) ->
    % 这里可以根据实际配置返回端口
    8888. 