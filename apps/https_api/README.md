# HTTPS API 模块

这个模块提供了HTTPS接口来实现后台管理和SDK登录等功能。

## 功能特性

### SDK认证接口
- **POST** `/api/v1/sdk/login` - SDK用户登录
- **POST** `/api/v1/sdk/verify` - 验证访问令牌
- **POST** `/api/v1/sdk/refresh` - 刷新访问令牌

### 后台管理接口
- **GET** `/api/v1/admin/stats` - 获取系统统计信息
- **GET/POST** `/api/v1/admin/players` - 玩家管理
- **GET/POST** `/api/v1/admin/guilds` - 公会管理
- **GET/POST** `/api/v1/admin/items` - 物品管理
- **GET** `/api/v1/admin/system` - 获取系统信息
- **GET** `/api/v1/admin/logs` - 查询系统日志

## 配置

在 `https_api.app.src` 中配置：

```erlang
{env, [
    {port, 8443},                    % HTTPS端口
    {cert_file, "priv/cert.pem"},    % SSL证书文件
    {key_file, "priv/key.pem"},      % SSL私钥文件
    {cacert_file, "priv/cacert.pem"} % CA证书文件
]}
```

## 使用方法

### 1. 启动应用

```erlang
application:start(https_api).
```

### 2. SDK登录示例

```bash
curl -X POST https://localhost:8443/api/v1/sdk/login \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"123456"}' \
  -k
```

### 3. 获取系统统计

```bash
curl -X GET https://localhost:8443/api/v1/admin/stats \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -k
```

### 4. 创建玩家

```bash
curl -X POST https://localhost:8443/api/v1/admin/players \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -d '{"username":"newplayer","nickname":"新玩家","level":1}' \
  -k
```

## 安全说明

- 默认使用自签名SSL证书（仅用于开发测试）
- 生产环境请使用正式的SSL证书
- 支持TLS 1.2和1.3版本
- 实现了访问令牌和刷新令牌机制

## 开发说明

### 模块结构
- `https_api_app.erl` - 应用主模块
- `https_api_sup.erl` - 监督者模块
- `https_server.erl` - HTTPS服务器
- `https_handler.erl` - 请求处理器
- `sdk_auth.erl` - SDK认证模块
- `admin_api.erl` - 后台管理API
- `https_init.erl` - 初始化模块

### 扩展新接口

1. 在 `https_handler.erl` 中添加新的路由
2. 创建对应的处理模块
3. 在 `admin_api.erl` 或 `sdk_auth.erl` 中实现具体逻辑

### 测试

```erlang
% 在Erlang shell中运行
c(test_https_api).
test_https_api:demo().
```

## 注意事项

- 需要安装OpenSSL来生成SSL证书
- 首次运行会自动生成自签名证书
- 默认监听8443端口
- 支持JSON格式的请求和响应
- 实现了完整的错误处理和状态码 