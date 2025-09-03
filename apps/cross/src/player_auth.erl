-module(player_auth).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([login/2, register/3, authenticate/1, logout/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SESSION_TIMEOUT, 3600). % 1小时超时

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% 认证接口
login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

register(Username, Password, Nickname) ->
    gen_server:call(?MODULE, {register, Username, Password, Nickname}).

authenticate(SessionId) ->
    gen_server:call(?MODULE, {authenticate, SessionId}).

logout(SessionId) ->
    gen_server:call(?MODULE, {logout, SessionId}).

%% gen_server callbacks
init([]) ->
    %% 初始化会话存储
    {ok, #{sessions => #{}}}.

handle_call({login, Username, Password}, _From, State) ->
    Result = do_login(Username, Password, State),
    {reply, Result, State};
handle_call({register, Username, Password, Nickname}, _From, State) ->
    Result = do_register(Username, Password, Nickname, State),
    {reply, Result, State};
handle_call({authenticate, SessionId}, _From, State) ->
    Result = do_authenticate(SessionId, State),
    {reply, Result, State};
handle_call({logout, SessionId}, _From, State) ->
    NewState = do_logout(SessionId, State),
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数
do_login(Username, Password, State) ->
    %% 这里应该查询数据库验证用户名和密码
    %% 现在用模拟实现
    case check_credentials(Username, Password) of
        {ok, RoleId} ->
            %% 使用ID生成器创建会话ID
            case id_generator:generate_session_id() of
                {ok, SessionId} ->
                    SessionData = #{
                        role_id => RoleId,
                        username => Username,
                        login_time => erlang:system_time(seconds),
                        expire_time => erlang:system_time(seconds) + ?SESSION_TIMEOUT
                    },
                    NewSessions = maps:put(SessionId, SessionData, maps:get(sessions, State)),
                    
                    %% 更新最后登录时间
                    mdb:update_player(#{role_id => RoleId, last_login_time => erlang:system_time(seconds)}),
                    
                    logger:log_info("玩家登录成功: ~s (RoleId: ~p, SessionId: ~p)", [Username, RoleId, SessionId]),
                    {ok, #{session_id => SessionId, role_id => RoleId}};
                {error, Reason} ->
                    logger:log_error("生成会话ID失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:log_warning("玩家登录失败: ~s, 原因: ~p", [Username, Reason]),
            {error, Reason}
    end.

do_register(Username, Password, Nickname, State) ->
    %% 检查用户名是否已存在
    case check_username_exists(Username) of
        {ok, exists} ->
            {error, username_already_exists};
        {ok, not_exists} ->
            %% 创建新玩家（role_id将由MDB模块通过ID生成器生成）
            PasswordHash = hash_password(Password),
            
            PlayerData = #{
                username => Username,
                password_hash => PasswordHash,
                nickname => Nickname
            },
            
            case mdb:create_player(PlayerData) of
                {ok, #{role_id := RoleId, username := Username}} ->
                    logger:log_info("玩家注册成功: ~s (RoleId: ~p)", [Username, RoleId]),
                    {ok, #{role_id => RoleId, username => Username}};
                {error, Reason} ->
                    logger:log_error("玩家注册失败: ~s, 原因: ~p", [Username, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

do_authenticate(SessionId, State) ->
    Sessions = maps:get(sessions, State),
    case maps:find(SessionId, Sessions) of
        {ok, SessionData} ->
            CurrentTime = erlang:system_time(seconds),
            ExpireTime = maps:get(expire_time, SessionData),
            
            if
                CurrentTime < ExpireTime ->
                    %% 会话有效，延长过期时间
                    NewSessionData = SessionData#{expire_time => CurrentTime + ?SESSION_TIMEOUT},
                    NewSessions = maps:put(SessionId, NewSessionData, Sessions),
                    {ok, SessionData};
                true ->
                    %% 会话已过期，删除
                    NewSessions = maps:remove(SessionId, Sessions),
                    {error, session_expired}
            end;
        error ->
            {error, session_not_found}
    end.

do_logout(SessionId, State) ->
    Sessions = maps:get(sessions, State),
    NewSessions = maps:remove(SessionId, Sessions),
    State#{sessions => NewSessions}.

%% 辅助函数
check_credentials(Username, Password) ->
    %% 这里应该查询数据库验证
    %% 现在用模拟实现
    if
        Username =:= "admin" andalso Password =:= "123456" ->
            {ok, 1000};
        Username =:= "test" andalso Password =:= "test123" ->
            {ok, 1001};
        true ->
            {error, invalid_credentials}
    end.

check_username_exists(Username) ->
    %% 这里应该查询数据库检查
    %% 现在用模拟实现
    if
        Username =:= "admin" orelse Username =:= "test" ->
            {ok, exists};
        true ->
            {ok, not_exists}
    end.

hash_password(Password) ->
    %% 这里应该使用实际的密码哈希算法
    %% 现在用简单实现
    erlang:md5(Password).

generate_session_id() ->
    %% 生成唯一的会话ID
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    erlang:integer_to_list(Timestamp) ++ "_" ++ erlang:integer_to_list(Random). 