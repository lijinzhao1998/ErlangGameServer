-module(sdk_auth).

-export([handle_login/1, handle_verify_token/1, handle_refresh_token/1]).

%% SDK登录接口
handle_login(#{method := "POST"} = Request) ->
    try
        Body = extract_body(Request),
        #{<<"username">> := Username, <<"password">> := Password} = jsx:decode(Body),
        
        case authenticate_sdk_user(Username, Password) of
            {ok, #{role_id := RoleId, session_id := SessionId, access_token := AccessToken, refresh_token := RefreshToken}} ->
                #{
                    status => 200,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => true,
                        data => #{
                            role_id => RoleId,
                            session_id => SessionId,
                            access_token => AccessToken,
                            refresh_token => RefreshToken,
                            expires_in => 3600
                        }
                    })
                };
            {error, Reason} ->
                #{
                    status => 401,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => false,
                        error => "Authentication failed",
                        message => Reason
                    })
                }
        end
    catch
        _:_Error ->
            #{
                status => 400,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Invalid request format",
                    message => "Missing required fields"
                })
            }
    end;

handle_login(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only POST method is supported"
        })
    }.

%% 验证访问令牌
handle_verify_token(#{method := "POST"} = Request) ->
    try
        Body = extract_body(Request),
        #{<<"access_token">> := AccessToken} = jsx:decode(Body),
        
        case verify_access_token(AccessToken) of
            {ok, #{role_id := RoleId, username := Username, permissions := Permissions}} ->
                #{
                    status => 200,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => true,
                        data => #{
                            role_id => RoleId,
                            username => Username,
                            permissions => Permissions,
                            valid => true
                        }
                    })
                };
            {error, Reason} ->
                #{
                    status => 401,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => false,
                        error => "Token verification failed",
                        message => Reason
                    })
                }
        end
    catch
        _:_Error ->
            #{
                status => 400,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Invalid request format",
                    message => "Missing access_token field"
                })
            }
    end;

handle_verify_token(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only POST method is supported"
        })
    }.

%% 刷新访问令牌
handle_refresh_token(#{method := "POST"} = Request) ->
    try
        Body = extract_body(Request),
        #{<<"refresh_token">> := RefreshToken} = jsx:decode(Body),
        
        case refresh_access_token(RefreshToken) of
            {ok, #{access_token := NewAccessToken, refresh_token := NewRefreshToken, expires_in := ExpiresIn}} ->
                #{
                    status => 200,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => true,
                        data => #{
                            access_token => NewAccessToken,
                            refresh_token => NewRefreshToken,
                            expires_in => ExpiresIn
                        }
                    })
                };
            {error, Reason} ->
                #{
                    status => 401,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => false,
                        error => "Token refresh failed",
                        message => Reason
                    })
                }
        end
    catch
        _:_Error ->
            #{
                status => 400,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Invalid request format",
                    message => "Missing refresh_token field"
                })
            }
    end;

handle_refresh_token(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only POST method is supported"
        })
    }.

%% 内部函数

extract_body(#{raw_data := RawData}) ->
    case string:split(RawData, "\r\n\r\n", all) of
        [_Headers, Body] -> Body;
        _ -> <<>>
    end.

authenticate_sdk_user(Username, Password) ->
    % 这里应该调用实际的认证逻辑
    % 暂时使用模拟数据
    case Username of
        <<"admin">> when Password =:= <<"123456">> ->
            RoleId = 1000,
            SessionId = generate_session_id(),
            AccessToken = generate_access_token(RoleId),
            RefreshToken = generate_refresh_token(RoleId),
            
            % 存储会话信息
            store_session(SessionId, #{
                role_id => RoleId,
                username => Username,
                access_token => AccessToken,
                refresh_token => RefreshToken,
                created_at => erlang:system_time(second)
            }),
            
            {ok, #{
                role_id => RoleId,
                session_id => SessionId,
                access_token => AccessToken,
                refresh_token => RefreshToken
            }};
        _ ->
            {error, "Invalid username or password"}
    end.

verify_access_token(AccessToken) ->
    % 这里应该验证实际的JWT令牌
    % 暂时使用模拟验证
    case get_session_by_token(AccessToken) of
        {ok, Session} ->
            {ok, Session};
        error ->
            {error, "Invalid or expired token"}
    end.

refresh_access_token(RefreshToken) ->
    % 这里应该验证刷新令牌并生成新的访问令牌
    case get_session_by_refresh_token(RefreshToken) of
        {ok, Session} ->
            NewAccessToken = generate_access_token(maps:get(role_id, Session)),
            NewRefreshToken = generate_refresh_token(maps:get(role_id, Session)),
            
            % 更新会话
            UpdatedSession = Session#{
                access_token => NewAccessToken,
                refresh_token => NewRefreshToken,
                updated_at => erlang:system_time(second)
            },
            update_session(maps:get(session_id, Session), UpdatedSession),
            
            {ok, #{
                access_token => NewAccessToken,
                refresh_token => NewRefreshToken,
                expires_in => 3600
            }};
        error ->
            {error, "Invalid refresh token"}
    end.

generate_session_id() ->
    crypto:strong_rand_bytes(16).

generate_access_token(_RoleId) ->
    % 这里应该生成实际的JWT令牌
    base64:encode(crypto:strong_rand_bytes(32)).

generate_refresh_token(_RoleId) ->
    % 这里应该生成实际的刷新令牌
    base64:encode(crypto:strong_rand_bytes(32)).

store_session(SessionId, SessionData) ->
    % 这里应该存储到数据库或缓存
    ets:insert(sdk_sessions, {SessionId, SessionData}).

get_session_by_token(AccessToken) ->
    % 这里应该从数据库或缓存中查找
    case ets:match_object(sdk_sessions, {'_', #{access_token => AccessToken}}) of
        [{_SessionId, Session}] -> {ok, Session};
        [] -> error
    end.

get_session_by_refresh_token(RefreshToken) ->
    % 这里应该从数据库或缓存中查找
    case ets:match_object(sdk_sessions, {'_', #{refresh_token => RefreshToken}}) of
        [{_SessionId, Session}] -> {ok, Session};
        [] -> error
    end.

update_session(SessionId, UpdatedSession) ->
    % 这里应该更新数据库或缓存
    ets:insert(sdk_sessions, {SessionId, UpdatedSession}). 