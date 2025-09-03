%% SSL-enabled login service that authenticates against Postgres (epgsql),
%% generates a session token, stores session, assigns a cross node and replies with protobuf LoginResp
%%
%% Notes:
%% - This example uses gpb runtime: gpb:encode/2 and gpb:decode/2 with type names "cross.Envelope", "cross.LoginReq", "cross.LoginResp".
%% - It uses epgsql (https://github.com/epgsql/epgsql) for Postgres access.
%% - TLS requires certificate/key files; the file paths are read from config/env.
%% - For production, store passwords hashed (bcrypt/argon2); this demo compares plaintext for brevity.

-module(login_service).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal functions (exported to suppress unused warnings)
-export([handle_envelope/5, handle_login/4, send_login_error/3, ssl_send_protobuf_response/3,
         check_user_credentials/3, choose_cross_node/1, store_session/6, epoch_to_pgts/1, make_uuid_v4/0]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    listen_ref,
    ssl_opts,
    pg_conn,
    cross_default_host = "127.0.0.1",
    cross_default_port = 6000
}).

%%% Public API
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

stop() ->
    gen_server:call(?MODULE, stop).

%%% gen_server callbacks
init(Opts) ->
    %% Read config from Opts or application env
    Port = proplists:get_value(port, Opts, 8443),
    CertFile = proplists:get_value(certfile, Opts, "/etc/ssl/certs/server.pem"),
    KeyFile  = proplists:get_value(keyfile, Opts, "/etc/ssl/private/server.key"),
    CAcert = proplists:get_value(cacert, Opts, undefined),

    %% Postgres connection options
    PGHost = proplists:get_value(pg_host, Opts, "127.0.0.1"),
    PGPort = proplists:get_value(pg_port, Opts, 5432),
    PGUser = proplists:get_value(pg_user, Opts, "postgres"),
    PGPass = proplists:get_value(pg_password, Opts, "postgres"),
    PGDB   = proplists:get_value(pg_db, Opts, "game"),

    %% SSL listen options (minimal TLS)
    SslOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {reuseaddr, true},
        {active, false}
    ] ++ (case CAcert of
            undefined -> [];
            File -> [{cacertfile, File}]
          end),

    {ok, ListenSocket} = ssl:listen(Port, SslOpts),
    ?LOG_INFO("login_service listening on port ~p (SSL)", [Port]),

    %% Connect to Postgres
    {ok, PgConn} = epgsql:connect(PGHost, PGPort, [{user, PGUser}, {password, PGPass}, {database, PGDB}]),
    ?LOG_INFO("Connected to Postgres at ~s:~p", [PGHost, PGPort]),

    %% spawn acceptor loop
    spawn_link(fun() -> accept_loop(ListenSocket, PgConn) end),

    {ok, #state{listen_ref=ListenSocket, ssl_opts=SslOpts, pg_conn=PgConn}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unsupported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.listen_ref of
        undefined -> ok;
        Sock -> ssl:close(Sock)
    end,
    ok.

code_change(_OldV, State, _Extra) ->
    {ok, State}.

%%% Accept loop
accept_loop(ListenSocket, PgConn) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, S} ->
            %% Handshake (sync)
            case ssl:handshake(S, [{timeout, 5000}]) of
                ok ->
                    spawn_link(fun() -> handle_connection(S, PgConn) end),
                    accept_loop(ListenSocket, PgConn);
                {error, Reason} ->
                    ?LOG_ERROR("SSL accept failed: ~p", [Reason]),
                    ssl:close(S),
                    accept_loop(ListenSocket, PgConn)
            end;
        {error, closed} ->
            ?LOG_INFO("listen socket closed"), ok;
        {error, Reason} ->
            ?LOG_ERROR("transport_accept error: ~p", [Reason]),
            accept_loop(ListenSocket, PgConn)
    end.

handle_connection(Socket, PgConn) ->
    ?LOG_INFO("New SSL connection ~p", [Socket]),
    loop_recv(Socket, PgConn),
    ssl:close(Socket),
    ?LOG_INFO("Connection ~p closed", [Socket]).

loop_recv(Socket, PgConn) ->
    case frame_pb_ssl:recv_and_unpack_ssl(Socket) of
        {ok, #{header := Header, payload := Payload}} ->
            Serializer = maps:get(serializer, Header),
            MsgType = maps:get(msg_type, Header),
            MsgId = maps:get(msg_id, Header),
            %% Only handle protobuf serializer here (1)
            case Serializer of
                1 ->
                    %% Decode Envelope
                    case safe_gpb_decode("cross.Envelope", Payload) of
                        {ok, EnvelopeMap} ->
                            handle_envelope(Socket, PgConn, MsgId, MsgType, EnvelopeMap),
                            loop_recv(Socket, PgConn);
                        {error, Reason} ->
                            ?LOG_ERROR("Envelope decode error: ~p", [Reason]),
                            ok
                    end;
                _Other ->
                    ?LOG_ERROR("Unsupported serializer: ~p", [_Other]),
                    ok
            end;
        {error, closed} ->
            ?LOG_INFO("peer closed");
        {error, Reason} ->
            ?LOG_ERROR("recv error: ~p", [Reason])
    end.

handle_envelope(Socket, PgConn, MsgId, MsgType, EnvelopeMap) ->
    Route = maps:get(route, EnvelopeMap, <<"">>),
    Body = maps:get(body, EnvelopeMap, <<>>),
    case Route of
        "account.login" ->
            handle_login(Socket, PgConn, MsgId, Body);
        _ ->
            ?LOG_WARNING("Unknown route ~p", [Route]),
            ok
    end.

handle_login(Socket, PgConn, MsgId, BodyBin) ->
    case safe_gpb_decode("cross.LoginReq", BodyBin) of
        {ok, ReqMap} ->
            Username = maps:get(username, ReqMap),
            Password = maps:get(password, ReqMap),
            case check_user_credentials(PgConn, Username, Password) of
                {ok, Uid} ->
                    %% choose a cross node
                    {Host, Port} = choose_cross_node(PgConn),
                    SessionUUID = make_uuid_v4(),
                    ExpiresAt = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + 86400,
                    %% store session
                    ok = store_session(PgConn, SessionUUID, Uid, Host, Port, ExpiresAt),
                    %% build LoginResp
                    RespMap = #{
                        code => 0,
                        message => "ok",
                        session_token => SessionUUID,
                        cross_host => Host,
                        cross_port => Port,
                        expires_at => ExpiresAt
                    },
                    BinResp = gpb:encode("cross.LoginResp", RespMap),
                    Envelope = #{msg_id => MsgId, msg_type => 1, route => <<"account.login_resp">>, body => BinResp},
                    EnvelopeBin = gpb:encode("cross.Envelope", Envelope),
                    ok = ssl_send_protobuf_response(Socket, MsgId, EnvelopeBin),
                    ?LOG_INFO("User ~p login success, assigned ~p:~p, session ~p", [Username, Host, Port, SessionUUID]);
                {error, Reason} ->
                    send_login_error(Socket, MsgId, Reason)
            end;
        {error, Reason} ->
            ?LOG_ERROR("LoginReq decode error: ~p", [Reason]),
            send_login_error(Socket, MsgId, <<"decode_error">>)
    end.

send_login_error(Socket, MsgId, Reason) ->
    RespMap = #{code => 1, message => io_lib:format("~p", [Reason])},
    BinResp = gpb:encode("cross.LoginResp", RespMap),
    Envelope = #{msg_id => MsgId, msg_type => 1, route => <<"account.login_resp">>, body => BinResp},
    EnvelopeBin = gpb:encode("cross.Envelope", Envelope),
    ok = ssl_send_protobuf_response(Socket, MsgId, EnvelopeBin).

ssl_send_protobuf_response(Socket, MsgId, EnvelopeBin) ->
    %% pack and ssl:send
    case frame_pb_ssl:send_protobuf_ssl(Socket, MsgId, 1, 0, EnvelopeBin, 1) of
        ok -> ok;
        {error, Reason} -> ?LOG_ERROR("send error: ~p", [Reason]), {error, Reason}
    end.

%%% DB helpers (epgsql)
check_user_credentials(PgConn, Username, Password) ->
    %% WARNING: demo plaintext password compare. Use hashed password in production.
    Sql = "SELECT id FROM users WHERE username = $1 AND password = $2 LIMIT 1",
    case epgsql:squery(PgConn, Sql, [Username, Password]) of
        {ok, _, _, [{Id}]} -> {ok, Id};
        {ok, _, _, []} -> {error, <<"invalid_credentials">>};
        {error, Reason} -> {error, Reason}
    end.

choose_cross_node(PgConn) ->
    %% Pick cross node with smallest load. Fallback to default if none.
    Sql = "SELECT host, port FROM cross_nodes ORDER BY load ASC LIMIT 1",
    case epgsql:squery(PgConn, Sql, []) of
        {ok, _, _, [{Host, Port}]} -> {Host, Port};
        _ -> {"127.0.0.1", 6000}
    end.

store_session(PgConn, SessionUUID, Uid, Host, Port, ExpiresAtEpoch) ->
    ExpiresTs = epoch_to_pgts(ExpiresAtEpoch),
    Sql = "INSERT INTO sessions (session_id, uid, expires_at, assigned_host, assigned_port) VALUES ($1, $2, $3, $4, $5)",
    case epgsql:squery(PgConn, Sql, [SessionUUID, Uid, ExpiresTs, Host, Port]) of
        {ok, _, _, _} -> ok;
        {error, Reason} -> ?LOG_ERROR("store_session error: ~p", [Reason]), {error, Reason}
    end.

epoch_to_pgts(Epoch) ->
    %% epoch seconds -> timestamp with time zone string acceptable by Postgres
    {{Y,M,D}, {HH,MM,SS}} = calendar:gregorian_seconds_to_datetime(Epoch),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B+00", [Y,M,D,HH,MM,SS]).

%%% Utilities
safe_gpb_decode(Type, Bin) ->
    try
        Map = gpb:decode(Type, Bin),
        {ok, Map}
    catch
        _:Reason -> {error, Reason}
    end.

make_uuid_v4() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    %% Set version 4 and variant bits per RFC 4122
    C1 = (C band 16#0fff) bor 16#4000,
    D1 = (D band 16#3fff) bor 16#8000,
    UUID = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
            [A, B, C1, D1, E]),
    lists:flatten(UUID).

%% read/send wrappers are in frame_pb_ssl (we use that for packing/unpacking)