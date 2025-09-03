-module(https_init).

-export([init/0, create_ssl_certs/0]).

init() ->
    % 创建ETS表用于存储SDK会话
    ets:new(sdk_sessions, [set, public, named_table]),
    
    % 创建SSL证书（如果不存在）
    create_ssl_certs(),
    
    % 初始化日志
    lager:info("HTTPS API initialized successfully"),
    ok.

create_ssl_certs() ->
    PrivDir = "priv",
    CertFile = filename:join(PrivDir, "cert.pem"),
    KeyFile = filename:join(PrivDir, "key.pem"),
    CacertFile = filename:join(PrivDir, "cacert.pem"),
    
    % 检查priv目录是否存在
    case filelib:is_dir(PrivDir) of
        false ->
            file:make_dir(PrivDir);
        true ->
            ok
    end,
    
    % 生成自签名证书（仅用于开发测试）
    case filelib:is_file(CertFile) andalso filelib:is_file(KeyFile) of
        true ->
            lager:info("SSL certificates already exist");
        false ->
            lager:info("Generating self-signed SSL certificates for development"),
            generate_self_signed_cert(CertFile, KeyFile, CacertFile)
    end.

generate_self_signed_cert(CertFile, KeyFile, CacertFile) ->
    % 生成私钥
    KeyCmd = io_lib:format("openssl genrsa -out ~s 2048", [KeyFile]),
    case os:cmd(KeyCmd) of
        [] ->
            % 生成自签名证书
            CertCmd = io_lib:format("openssl req -new -x509 -key ~s -out ~s -days 365 -subj \"/C=CN/ST=Beijing/L=Beijing/O=GameServer/CN=localhost\"", [KeyFile, CertFile]),
            case os:cmd(CertCmd) of
                [] ->
                    % 复制证书作为CA证书
                    file:copy(CertFile, CacertFile),
                    lager:info("SSL certificates generated successfully");
                Error ->
                    lager:error("Failed to generate certificate: ~s", [Error])
            end;
        Error ->
            lager:error("Failed to generate private key: ~s", [Error])
    end. 