-module(admin_api).

-export([handle_stats/1, handle_players/1, handle_guilds/1, handle_items/1, handle_system_info/1, handle_logs/1]).

%% 系统统计信息
handle_stats(#{method := "GET"} = _Request) ->
    try
        Stats = collect_system_stats(),
        #{
            status => 200,
            headers => [{"Content-Type", "application/json"}],
            body => jsx:encode(#{
                success => true,
                data => Stats
            })
        }
    catch
        _:_Error ->
            #{
                status => 500,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Failed to collect statistics",
                    message => "Internal server error"
                })
            }
    end;

handle_stats(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only GET method is supported"
        })
    }.

%% 玩家管理
handle_players(#{method := "GET"} = _Request) ->
    try
        Players = get_players_list(),
        #{
            status => 200,
            headers => [{"Content-Type", "application/json"}],
            body => jsx:encode(#{
                success => true,
                data => Players
            })
        }
    catch
        _:_Error ->
            #{
                status => 500,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Failed to get players list",
                    message => "Internal server error"
                })
            }
    end;

handle_players(#{method := "POST"} = Request) ->
    try
        Body = extract_body(Request),
        PlayerData = jsx:decode(Body),
        case create_player(PlayerData) of
            {ok, PlayerId} ->
                #{
                    status => 201,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => true,
                        data => #{player_id => PlayerId}
                    })
                };
            {error, Reason} ->
                #{
                    status => 400,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => false,
                        error => "Failed to create player",
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
                    message => "Invalid JSON data"
                })
            }
    end;

handle_players(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only GET and POST methods are supported"
        })
    }.

%% 公会管理
handle_guilds(#{method := "GET"} = _Request) ->
    try
        Guilds = get_guilds_list(),
        #{
            status => 200,
            headers => [{"Content-Type", "application/json"}],
            body => jsx:encode(#{
                success => true,
                data => Guilds
            })
        }
    catch
        _:_Error ->
            #{
                status => 500,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Failed to get guilds list",
                    message => "Internal server error"
                })
            }
    end;

handle_guilds(#{method := "POST"} = Request) ->
    try
        Body = extract_body(Request),
        GuildData = jsx:decode(Body),
        case create_guild(GuildData) of
            {ok, GuildId} ->
                #{
                    status => 201,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => true,
                        data => #{guild_id => GuildId}
                    })
                };
            {error, Reason} ->
                #{
                    status => 400,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => false,
                        error => "Failed to create guild",
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
                    message => "Invalid JSON data"
                })
            }
    end;

handle_guilds(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only GET and POST methods are supported"
        })
    }.

%% 物品管理
handle_items(#{method := "GET"} = _Request) ->
    try
        Items = get_items_list(),
        #{
            status => 200,
            headers => [{"Content-Type", "application/json"}],
            body => jsx:encode(#{
                success => true,
                data => Items
            })
        }
    catch
        _:_Error ->
            #{
                status => 500,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Failed to get items list",
                    message => "Internal server error"
                })
            }
    end;

handle_items(#{method := "POST"} = Request) ->
    try
        Body = extract_body(Request),
        ItemData = jsx:decode(Body),
        case create_item(ItemData) of
            {ok, ItemId} ->
                #{
                    status => 201,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => true,
                        data => #{item_id => ItemId}
                    })
                };
            {error, Reason} ->
                #{
                    status => 400,
                    headers => [{"Content-Type", "application/json"}],
                    body => jsx:encode(#{
                        success => false,
                        error => "Failed to create item",
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
                    message => "Invalid JSON data"
                })
            }
    end;

handle_items(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only GET and POST methods are supported"
        })
    }.

%% 系统信息
handle_system_info(#{method := "GET"} = _Request) ->
    try
        SystemInfo = collect_system_info(),
        #{
            status => 200,
            headers => [{"Content-Type", "application/json"}],
            body => jsx:encode(#{
                success => true,
                data => SystemInfo
            })
        }
    catch
        _:_Error ->
            #{
                status => 500,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Failed to get system info",
                    message => "Internal server error"
                })
            }
    end;

handle_system_info(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only GET method is supported"
        })
    }.

%% 日志查询
handle_logs(#{method := "GET"} = Request) ->
    try
        QueryParams = extract_query_params(Request),
        Logs = query_logs(QueryParams),
        #{
            status => 200,
            headers => [{"Content-Type", "application/json"}],
            body => jsx:encode(#{
                success => true,
                data => Logs
            })
        }
    catch
        _:_Error ->
            #{
                status => 500,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{
                    success => false,
                    error => "Failed to query logs",
                    message => "Internal server error"
                })
            }
    end;

handle_logs(_) ->
    #{
        status => 405,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            success => false,
            error => "Method not allowed",
            message => "Only GET method is supported"
        })
    }.

%% 内部函数

extract_body(#{raw_data := RawData}) ->
    case string:split(RawData, "\r\n\r\n", all) of
        [_Headers, Body] -> Body;
        _ -> <<>>
    end.

extract_query_params(#{raw_data := RawData}) ->
    % 简单的查询参数解析
    case string:split(RawData, "?", all) of
        [_Path, QueryString] ->
            parse_query_string(QueryString);
        _ ->
            #{}
    end.

parse_query_string(QueryString) ->
    % 简单的查询字符串解析
    Params = string:split(QueryString, "&", all),
    lists:foldl(fun(Param, Acc) ->
        case string:split(Param, "=", all) of
            [Key, Value] -> Acc#{Key => Value};
            _ -> Acc
        end
    end, #{}, Params).

collect_system_stats() ->
    % 收集系统统计信息
    #{
        total_players => get_total_players_count(),
        online_players => get_online_players_count(),
        total_guilds => get_total_guilds_count(),
        total_items => get_total_items_count(),
        system_uptime => get_system_uptime(),
        memory_usage => get_memory_usage(),
        cpu_usage => get_cpu_usage()
    }.

get_players_list() ->
    % 获取玩家列表
    % 这里应该调用实际的数据库查询
    [
        #{id => 1000, username => "admin", nickname => "管理员", level => 50, exp => 100000},
        #{id => 1001, username => "player1", nickname => "玩家1", level => 30, exp => 50000},
        #{id => 1002, username => "player2", nickname => "玩家2", level => 25, exp => 30000}
    ].

get_guilds_list() ->
    % 获取公会列表
    % 这里应该调用实际的数据库查询
    [
        #{id => 40001, name => "测试公会1", leader => "admin", member_count => 10, level => 5},
        #{id => 40002, name => "测试公会2", leader => "player1", member_count => 5, level => 3}
    ].

get_items_list() ->
    % 获取物品列表
    % 这里应该调用实际的数据库查询
    [
        #{id => 50001, name => "铁剑", type => "weapon", quality => 3, level => 10},
        #{id => 50002, name => "皮甲", type => "armor", quality => 2, level => 5},
        #{id => 50003, name => "生命药水", type => "consumable", quality => 1, level => 1}
    ].

collect_system_info() ->
    % 收集系统信息
    #{
        version => "1.0.0",
        erlang_version => erlang:system_info(otp_release),
        node_name => node(),
        uptime => get_system_uptime(),
        memory => get_memory_usage(),
        processes => erlang:system_info(process_count),
        ports => erlang:system_info(port_count)
    }.

query_logs(_QueryParams) ->
    % 查询日志
    % 这里应该调用实际的日志查询
    [
        #{timestamp => "2024-01-01 10:00:00", level => "info", message => "系统启动"},
        #{timestamp => "2024-01-01 10:01:00", level => "info", message => "玩家admin登录"},
        #{timestamp => "2024-01-01 10:02:00", level => "warning", message => "数据库连接延迟"}
    ].

create_player(_PlayerData) ->
    % 创建玩家
    % 这里应该调用实际的数据库操作
    {ok, 1003}.

create_guild(_GuildData) ->
    % 创建公会
    % 这里应该调用实际的数据库操作
    {ok, 40003}.

create_item(_ItemData) ->
    % 创建物品
    % 这里应该调用实际的数据库操作
    {ok, 50004}.

get_total_players_count() -> 1000.
get_online_players_count() -> 150.
get_total_guilds_count() -> 50.
get_total_items_count() -> 5000.
get_system_uptime() -> 86400.
get_memory_usage() -> 1024.
get_cpu_usage() -> 25. 