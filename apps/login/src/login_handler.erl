%% Very small connection handler: reads lines and responds.
-module(login_handler).
-export([serve/1]).

serve(Socket) ->
    ok = gen_tcp:send(Socket, "Welcome to login server\r\n"),
    loop(Socket),
    gen_tcp:close(Socket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            Line = binary_to_list(Bin),
            handle_line(string:trim(Line), Socket),
            gen_tcp:send(Socket, "OK\r\n"),
            loop(Socket);
        {error, closed} ->
            io:format("Client closed~n"),
            ok;
        {error, Reason} ->
            io:format("recv error ~p~n", [Reason]),
            ok
    end.

handle_line(Line, _Socket) ->
    io:format("Login received: ~s~n", [Line]),
    case string:tokens(Line, " ") of
        ["login", User, Pass] ->
            io:format("Auth attempt user=~s pass=~s~n", [User, Pass]),
            %% TODO: real auth -> create token via login_session_manager
            ok;
        _ ->
            ok
    end.