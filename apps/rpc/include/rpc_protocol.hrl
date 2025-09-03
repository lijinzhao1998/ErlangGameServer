%% RPC协议头文件
%% 包含消息结构定义和常量

%% 消息类型常量
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