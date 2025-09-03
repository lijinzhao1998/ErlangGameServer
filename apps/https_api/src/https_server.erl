-module(https_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listen_socket, port, cert_file, key_file, cacert_file}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Port = application:get_env(https_api, port, 8443),
    CertFile = application:get_env(https_api, cert_file, "priv/cert.pem"),
    KeyFile = application:get_env(https_api, key_file, "priv/key.pem"),
    CacertFile = application:get_env(https_api, cacert_file, "priv/cacert.pem"),
    
    State = #state{port = Port, cert_file = CertFile, key_file = KeyFile, cacert_file = CacertFile},
    
    case start_listener(State) of
        {ok, ListenSocket} ->
            {ok, State#state{listen_socket = ListenSocket}};
        {error, Reason} ->
            {stop, Reason}
    end.

start_listener(#state{port = Port, cert_file = CertFile, key_file = KeyFile, cacert_file = CacertFile}) ->
    SSLOptions = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {cacertfile, CacertFile},
        {verify, verify_peer},
        {fail_if_no_peer_cert, false},
        {verify_fun, {fun verify_peer_cert/3, []}},
        {reuse_sessions, true},
        {honor_cipher_order, true},
        {versions, ['tlsv1.2', 'tlsv1.3']}
    ],
    
    ssl:listen(Port, [{active, false}, {packet, http}, {reuseaddr, true} | SSLOptions]).

verify_peer_cert(Cert, Event, InitialUserState) ->
    case Event of
        {bad_cert, _Reason} ->
            {valid, InitialUserState};
        {extension, _} ->
            {unknown, InitialUserState};
        valid ->
            {valid, InitialUserState};
        valid_peer ->
            {valid, InitialUserState}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ssl, Socket, Data}, State) ->
    spawn(fun() -> handle_https_request(Socket, Data) end),
    ssl:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({ssl_closed, _Socket}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listen_socket = ListenSocket}) ->
    case ListenSocket of
        undefined -> ok;
        _ -> ssl:close(ListenSocket)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_https_request(Socket, Data) ->
    try
        {ok, Request} = parse_https_request(Data),
        Response = https_handler:handle_request(Request),
        send_https_response(Socket, Response)
    catch
        _:Error ->
            lager:error("Error handling HTTPS request: ~p", [Error]),
            ErrorResponse = #{
                status => 500,
                headers => [{"Content-Type", "application/json"}],
                body => jsx:encode(#{error => "Internal Server Error"})
            },
            send_https_response(Socket, ErrorResponse)
    end.

parse_https_request(Data) ->
    % 简单的HTTP请求解析
    Lines = string:split(Data, "\r\n", all),
    [RequestLine | HeaderLines] = Lines,
    [Method, Path, _Version] = string:split(RequestLine, " ", all),
    
    Headers = parse_headers(HeaderLines),
    
    {ok, #{
        method => Method,
        path => Path,
        headers => Headers,
        raw_data => Data
    }}.

parse_headers([]) -> [];
parse_headers(["" | _]) -> [];
parse_headers([Line | Rest]) ->
    case string:split(Line, ": ", all) of
        [Key, Value] -> [{Key, Value} | parse_headers(Rest)];
        _ -> parse_headers(Rest)
    end.

send_https_response(Socket, #{status := Status, headers := Headers, body := Body}) ->
    StatusLine = io_lib:format("HTTP/1.1 ~p ~s\r\n", [Status, status_text(Status)]),
    HeaderLines = [io_lib:format("~s: ~s\r\n", [K, V]) || {K, V} <- Headers],
    Response = [StatusLine, HeaderLines, "\r\n", Body],
    ssl:send(Socket, Response).

status_text(200) -> "OK";
status_text(201) -> "Created";
status_text(400) -> "Bad Request";
status_text(401) -> "Unauthorized";
status_text(403) -> "Forbidden";
status_text(404) -> "Not Found";
status_text(500) -> "Internal Server Error";
status_text(_) -> "Unknown". 