-- 示例 migration，用于 demo。生产环境请使用密码哈希与合适索引/约束。
-- 需要在 PostgreSQL 中运行此 SQL 来创建示例表。

CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  username TEXT UNIQUE NOT NULL,
  password TEXT NOT NULL,   -- 示例：明文/测试用，生产请使用密码哈希（bcrypt/argon2）与 salt
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now()
);

CREATE TABLE IF NOT EXISTS cross_nodes (
  id SERIAL PRIMARY KEY,
  host TEXT NOT NULL,
  port INTEGER NOT NULL,
  load INTEGER DEFAULT 0,   -- 简单的负载指标（示例）
  last_heartbeat TIMESTAMP WITH TIME ZONE DEFAULT now()
);

CREATE TABLE IF NOT EXISTS sessions (
  session_id UUID PRIMARY KEY,
  uid INTEGER REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now(),
  expires_at TIMESTAMP WITH TIME ZONE,
  assigned_host TEXT,
  assigned_port INTEGER
);

-- 示例插入（你可以根据实际部署填真实 cross 节点）
INSERT INTO cross_nodes (host, port, load) VALUES ('127.0.0.1', 6000, 0) ON CONFLICT DO NOTHING;