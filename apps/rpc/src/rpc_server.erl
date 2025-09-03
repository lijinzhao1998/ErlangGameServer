%% RPC服务器模块
-module(rpc_server).
-behaviour(gen_server).

%% 包含协议定义
-include("rpc_protocol.hrl").

-export([
    start_link/0,
    start_listener/1,
    stop_listener/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    listener_socket,
    port,
    connections = #{},
    max_connections,
    connection_timeout
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_listener(Port) ->
    gen_server:call(?MODULE, {start_listener, Port}).

stop_listener() ->
    gen_server:call(?MODULE, stop_listener).

init([]) ->
    Port = application:get_env(rpc, default_port, 8888),
    MaxConnections = application:get_env(rpc, max_connections, 1000),
    ConnectionTimeout = application:get_env(rpc, connection_timeout, 10000),
    
    State = #state{
        port = Port,
        max_connections = MaxConnections,
        connection_timeout = ConnectionTimeout
    },
    
    {ok, State}.

handle_call({start_listener, Port}, _From, State) ->
    case start_listener_socket(Port) of
        {ok, ListenSocket} ->
            io:format("RPC server listening on port ~p~n", [Port]),
            {reply, ok, State#state{listener_socket = ListenSocket, port = Port}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(stop_listener, _From, State = #state{listener_socket = ListenSocket}) ->
    case ListenSocket of
        undefined -> ok;
        _ -> gen_tcp:close(ListenSocket)
    end,
    {reply, ok, State#state{listener_socket = undefined}};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    handle_client_message(Socket, Data, State),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    handle_client_disconnect(Socket, State),
    {noreply, State};

handle_info({tcp_error, Socket, Reason}, State) ->
    io:format("TCP error on socket ~p: ~p~n", [Socket, Reason]),
    handle_client_disconnect(Socket, State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{listener_socket = ListenSocket}) ->
    case ListenSocket of
        undefined -> ok;
        _ -> gen_tcp:close(ListenSocket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 启动监听socket
start_listener_socket(Port) ->
    Options = [
        binary,
        {packet, 0},
        {active, true},
        {reuseaddr, true},
        {backlog, 128}
    ],
    gen_tcp:listen(Port, Options).

%% 处理客户端消息
handle_client_message(Socket, Data, _State) ->
    case rpc_protocol:decode_message(Data) of
        #rpc_message{} = Message ->
            process_rpc_message(Socket, Message);
        {error, Reason} ->
            io:format("Failed to decode message: ~p~n", [Reason]),
            send_error_response(Socket, "Invalid message format");
        _ ->
            io:format("Invalid message format~n"),
            send_error_response(Socket, "Invalid message format")
    end.

%% 处理RPC消息
process_rpc_message(Socket, Message) ->
    case rpc_protocol:get_message_type(Message) of
        1 -> % REQUEST
            handle_request(Socket, Message);
        2 -> % RESPONSE
            handle_response(Socket, Message);
        3 -> % NOTIFICATION
            handle_notification(Socket, Message);
        4 -> % HEARTBEAT
            handle_heartbeat(Socket, Message);
        _ ->
            send_error_response(Socket, "Unknown message type")
    end.

%% 处理请求消息
handle_request(Socket, #rpc_message{id = Id, service = Service, method = Method, params = Params}) ->
    try
        % 这里应该根据服务和方法调用相应的处理函数
        Result = call_service_method(Service, Method, Params),
        Response = rpc_protocol:create_response(Id, Result, undefined),
        send_response(Socket, Response)
    catch
        _:_Error ->
            ErrorResponse = rpc_protocol:create_response(Id, undefined, "Internal error"),
            send_response(Socket, ErrorResponse)
    end.

%% 处理响应消息
handle_response(_Socket, _Message) ->
    % 响应消息通常由客户端处理
    ok.

%% 处理通知消息
handle_notification(Socket, #rpc_message{service = Service, method = Method}) ->
    try
        % 处理通知
        handle_service_notification(Service, Method),
        send_ack(Socket)
    catch
        _:_Error ->
            send_error_response(Socket, "Notification failed")
    end.

%% 处理心跳消息
handle_heartbeat(Socket, _Message) ->
    % 发送心跳响应
    HeartbeatResponse = rpc_protocol:create_heartbeat(),
    send_response(Socket, HeartbeatResponse).

%% 调用服务方法
call_service_method(Service, Method, Params) ->
    % 这里应该实现实际的服务调用逻辑
    % 暂时返回模拟结果
    #{service => Service, method => Method, params => Params, result => "success"}.

%% 处理服务通知
handle_service_notification(Service, Method) ->
    io:format("Received notification from ~s: ~s~n", [Service, Method]).

%% 发送响应
send_response(Socket, Response) ->
    case rpc_protocol:encode_message(Response) of
        {ok, EncodedData} ->
            gen_tcp:send(Socket, EncodedData);
        {error, _Reason} ->
            send_error_response(Socket, "Failed to encode response");
        _ ->
            send_error_response(Socket, "Failed to encode response")
    end.

%% 发送错误响应
send_error_response(Socket, Error) ->
    ErrorResponse = rpc_protocol:create_response(0, undefined, Error),
    send_response(Socket, ErrorResponse).

%% 发送确认
send_ack(Socket) ->
    AckResponse = rpc_protocol:create_response(0, "OK", undefined),
    send_response(Socket, AckResponse).

%% 处理客户端断开连接
handle_client_disconnect(Socket, _State) ->
    io:format("Client disconnected: ~p~n", [Socket]),
    gen_tcp:close(Socket),
    ok. 