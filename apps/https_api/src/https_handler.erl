-module(https_handler).

-export([handle_request/1]).

handle_request(#{method := Method, path := Path} = Request) ->
    case parse_path(Path) of
        {sdk, "login", _} ->
            sdk_auth:handle_login(Request);
        {sdk, "verify", _} ->
            sdk_auth:handle_verify_token(Request);
        {sdk, "refresh", _} ->
            sdk_auth:handle_refresh_token(Request);
        {admin, "stats", _} ->
            admin_api:handle_stats(Request);
        {admin, "players", _} ->
            admin_api:handle_players(Request);
        {admin, "guilds", _} ->
            admin_api:handle_guilds(Request);
        {admin, "items", _} ->
            admin_api:handle_items(Request);
        {admin, "system", _} ->
            admin_api:handle_system_info(Request);
        {admin, "logs", _} ->
            admin_api:handle_logs(Request);
        _ ->
            not_found_response()
    end.

parse_path(Path) ->
    case string:split(Path, "/", all) of
        ["", "api", "v1", "sdk", Action | Rest] ->
            {sdk, Action, Rest};
        ["", "api", "v1", "admin", Action | Rest] ->
            {admin, Action, Rest};
        _ ->
            {unknown, "unknown", []}
    end.

not_found_response() ->
    #{
        status => 404,
        headers => [{"Content-Type", "application/json"}],
        body => jsx:encode(#{
            error => "Not Found",
            message => "The requested resource was not found"
        })
    }. 