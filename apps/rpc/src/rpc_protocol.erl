%% RPC协议处理模块
-module(rpc_protocol).
-export([
    encode_message/1,
    decode_message/1,
    create_request/4,
    create_response/3,
    create_notification/2,
    create_heartbeat/0,
    is_heartbeat/1,
    get_message_type/1
]).

%% 消息类型定义
-define(MSG_TYPE_REQUEST, 1).
-define(MSG_TYPE_RESPONSE, 2).
-define(MSG_TYPE_NOTIFICATION, 3).
-define(MSG_TYPE_HEARTBEAT, 4).

%% 消息结构
-record(rpc_message, {
    type,           % 消息类型
    id,             % 消息ID（请求/响应）
    service,        % 服务名称
    method,         % 方法名称
    params,         % 参数
    result,         % 结果（响应）
    error,          % 错误信息
    timestamp,      % 时间戳
    version         % 协议版本
}).

%% 创建请求消息
create_request(Service, Method, Params, Id) ->
    #rpc_message{
        type = ?MSG_TYPE_REQUEST,
        id = Id,
        service = Service,
        method = Method,
        params = Params,
        timestamp = erlang:system_time(millisecond),
        version = "1.0"
    }.

%% 创建响应消息
create_response(Id, Result, Error) ->
    #rpc_message{
        type = ?MSG_TYPE_RESPONSE,
        id = Id,
        result = Result,
        error = Error,
        timestamp = erlang:system_time(millisecond),
        version = "1.0"
    }.

%% 创建通知消息
create_notification(Service, Method) ->
    #rpc_message{
        type = ?MSG_TYPE_NOTIFICATION,
        service = Service,
        method = Method,
        timestamp = erlang:system_time(millisecond),
        version = "1.0"
    }.

%% 创建心跳消息
create_heartbeat() ->
    #rpc_message{
        type = ?MSG_TYPE_HEARTBEAT,
        timestamp = erlang:system_time(millisecond),
        version = "1.0"
    }.

%% 编码消息为二进制
encode_message(Message) ->
    try
        jsx:encode(message_to_map(Message))
    catch
        _:Error ->
            {error, {encode_failed, Error}}
    end.

%% 解码二进制为消息
decode_message(Bin) when is_binary(Bin) ->
    try
        Map = jsx:decode(Bin),
        map_to_message(Map)
    catch
        _:Error ->
            {error, {decode_failed, Error}}
    end;
decode_message(_) ->
    {error, invalid_input}.

%% 检查是否为心跳消息
is_heartbeat(#rpc_message{type = ?MSG_TYPE_HEARTBEAT}) -> true;
is_heartbeat(_) -> false.

%% 获取消息类型
get_message_type(#rpc_message{type = Type}) -> Type.

%% 内部函数：消息记录转Map
message_to_map(#rpc_message{} = Msg) ->
    #{
        type => Msg#rpc_message.type,
        id => Msg#rpc_message.id,
        service => Msg#rpc_message.service,
        method => Msg#rpc_message.method,
        params => Msg#rpc_message.params,
        result => Msg#rpc_message.result,
        error => Msg#rpc_message.error,
        timestamp => Msg#rpc_message.timestamp,
        version => Msg#rpc_message.version
    }.

%% 内部函数：Map转消息记录
map_to_message(Map) ->
    #rpc_message{
        type = maps:get(type, Map, 0),
        id = maps:get(id, Map, undefined),
        service = maps:get(service, Map, undefined),
        method = maps:get(method, Map, undefined),
        params = maps:get(params, Map, undefined),
        result = maps:get(result, Map, undefined),
        error = maps:get(error, Map, undefined),
        timestamp = maps:get(timestamp, Map, 0),
        version = maps:get(version, Map, "1.0")
    }. 