#!/bin/bash

echo "启动HTTPS API服务器..."

# 检查Erlang是否安装
if ! command -v erl &> /dev/null; then
    echo "错误: 未找到Erlang，请先安装Erlang/OTP"
    exit 1
fi

# 检查OpenSSL是否安装
if ! command -v openssl &> /dev/null; then
    echo "警告: 未找到OpenSSL，SSL证书生成可能失败"
fi

# 启动Erlang shell并加载HTTPS API
erl -pa apps/*/ebin -eval "
    % 启动HTTPS API应用
    application:start(https_api),
    
    % 等待应用启动
    timer:sleep(2000),
    
    % 显示启动信息
    io:format(\"~n=== HTTPS API 服务器已启动 ===~n\"),
    io:format(\"监听端口: 8443~n\"),
    io:format(\"SDK登录接口: https://localhost:8443/api/v1/sdk/login~n\"),
    io:format(\"后台管理接口: https://localhost:8443/api/v1/admin/stats~n\"),
    io:format(\"~n按 Ctrl+C 退出~n\"),
    
    % 保持shell运行
    receive
        _ -> ok
    end
" 