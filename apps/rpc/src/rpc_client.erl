%% RPC客户端模块
-module(rpc_client).
-behaviour(gen_server).

%% 包含协议定义
-include("rpc_protocol.hrl").

-export([
    start_link/2,
    call/3,
    call/4,
    cast/3,
    notify/2,
    send_heartbeat/0,
    disconnect/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    socket,
    server_address,
    server_port,
    message_id = 1,
    pending_requests = #{},
    heartbeat_timer,
    heartbeat_interval
}).

start_link(Address, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Address, Port], []).

%% 同步调用远程服务
call(Service, Method, Params) ->
    call(Service, Method, Params, 5000).

call(Service, Method, Params, Timeout) ->
    gen_server:call(?MODULE, {call, Service, Method, Params, Timeout}, Timeout + 1000).

%% 异步调用远程服务
cast(Service, Method, Params) ->
    gen_server:cast(?MODULE, {cast, Service, Method, Params}).

%% 发送通知
notify(Service, Method) ->
    gen_server:cast(?MODULE, {notify, Service, Method}).

%% 发送心跳
send_heartbeat() ->
    gen_server:cast(?MODULE, send_heartbeat).

%% 断开连接
disconnect() ->
    gen_server:call(?MODULE, disconnect).

init([Address, Port]) ->
    HeartbeatInterval = application:get_env(rpc, heartbeat_interval, 30000),
    
    State = #state{
        server_address = Address,
        server_port = Port,
        heartbeat_interval = HeartbeatInterval
    },
    
    case connect_to_server(Address, Port) of
        {ok, Socket} ->
            % 启动心跳定时器
            Timer = erlang:send_after(HeartbeatInterval, self(), send_heartbeat),
            {ok, State#state{socket = Socket, heartbeat_timer = Timer}};
        {error, Reason} ->
            {stop, {connection_failed, Reason}}
    end.

handle_call({call, Service, Method, Params, Timeout}, From, State) ->
    MessageId = State#state.message_id,
    Request = rpc_protocol:create_request(Service, Method, Params, MessageId),
    
    case send_request(Request, State#state.socket) of
        ok ->
            % 存储待处理的请求
            PendingRequests = maps:put(MessageId, {From, erlang:system_time(millisecond) + Timeout}, State#state.pending_requests),
            NewState = State#state{
                message_id = MessageId + 1,
                pending_requests = PendingRequests
            },
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(disconnect, _From, State) ->
    case State#state.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end,
    {reply, ok, State#state{socket = undefined}};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast({cast, Service, Method, Params}, State) ->
    MessageId = State#state.message_id,
    Request = rpc_protocol:create_request(Service, Method, Params, MessageId),
    send_request(Request, State#state.socket),
    {noreply, State#state{message_id = MessageId + 1}};

handle_cast({notify, Service, Method}, State) ->
    Notification = rpc_protocol:create_notification(Service, Method),
    send_notification(Notification, State#state.socket),
    {noreply, State};

handle_cast(send_heartbeat, State) ->
    Heartbeat = rpc_protocol:create_heartbeat(),
    send_heartbeat_message(Heartbeat, State#state.socket),
    
    % 重新启动心跳定时器
    Timer = erlang:send_after(State#state.heartbeat_interval, self(), send_heartbeat),
    {noreply, State#state{heartbeat_timer = Timer}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
    case rpc_protocol:decode_message(Data) of
        {ok, Message} ->
            handle_server_message(Message, State);
        {error, Reason} ->
            io:format("Failed to decode server message: ~p~n", [Reason]),
            {noreply, State}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    io:format("Connection to server closed~n"),
    {noreply, State#state{socket = undefined}};

handle_info({tcp_error, _Socket, Reason}, State) ->
    io:format("TCP error: ~p~n", [Reason]),
    {noreply, State#state{socket = undefined}};

handle_info(send_heartbeat, State) ->
    Heartbeat = rpc_protocol:create_heartbeat(),
    send_heartbeat_message(Heartbeat, State#state.socket),
    
    % 重新启动心跳定时器
    Timer = erlang:send_after(State#state.heartbeat_interval, self(), send_heartbeat),
    {noreply, State#state{heartbeat_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 连接到服务器
connect_to_server(Address, Port) ->
    Options = [
        binary,
        {packet, 0},
        {active, true},
        {reuseaddr, true}
    ],
    gen_tcp:connect(Address, Port, Options).

%% 发送请求
send_request(Request, Socket) ->
    case rpc_protocol:encode_message(Request) of
        {ok, EncodedData} ->
            gen_tcp:send(Socket, EncodedData);
        {error, Reason} ->
            {error, {encode_failed, Reason}}
    end.

%% 发送通知
send_notification(Notification, Socket) ->
    case rpc_protocol:encode_message(Notification) of
        {ok, EncodedData} ->
            gen_tcp:send(Socket, EncodedData);
        {error, Reason} ->
            {error, {encode_failed, Reason}}
    end.

%% 发送心跳消息
send_heartbeat_message(Heartbeat, Socket) ->
    case rpc_protocol:encode_message(Heartbeat) of
        {ok, EncodedData} ->
            gen_tcp:send(Socket, EncodedData);
        {error, _Reason} ->
            ok % 心跳失败不影响主流程
    end.

%% 处理服务器消息
handle_server_message(Message, State) ->
    case rpc_protocol:get_message_type(Message) of
        2 -> % RESPONSE
            handle_response(Message, State);
        4 -> % HEARTBEAT
            handle_heartbeat_response(Message),
            {noreply, State};
        _ ->
            io:format("Received unexpected message type: ~p~n", [Message]),
            {noreply, State}
    end.

%% 处理响应消息
handle_response(#rpc_message{id = Id, result = Result, error = Error}, State) ->
    case maps:find(Id, State#state.pending_requests) of
        {ok, {From, _Timeout}} ->
            % 回复等待的进程
            case Error of
                undefined ->
                    gen_server:reply(From, {ok, Result});
                _ ->
                    gen_server:reply(From, {error, Error})
            end,
            
            % 从待处理请求中移除
            PendingRequests = maps:remove(Id, State#state.pending_requests),
            {noreply, State#state{pending_requests = PendingRequests}};
        error ->
            io:format("Received response for unknown request ID: ~p~n", [Id]),
            {noreply, State}
    end.

%% 处理心跳响应
handle_heartbeat_response(_Message) ->
    % 心跳响应，可以记录或忽略
    ok. 