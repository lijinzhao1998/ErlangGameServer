-- 创建玩家表
CREATE TABLE IF NOT EXISTS players (
    role_id INTEGER PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    nickname VARCHAR(50),
    level INTEGER DEFAULT 1,
    exp INTEGER DEFAULT 0,
    gold INTEGER DEFAULT 0,
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_login_time TIMESTAMP,
    status INTEGER DEFAULT 1
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_players_username ON players(username);
CREATE INDEX IF NOT EXISTS idx_players_status ON players(status);
CREATE INDEX IF NOT EXISTS idx_players_level ON players(level);

-- 插入测试数据
INSERT OR IGNORE INTO players (role_id, username, password_hash, nickname, level, exp, gold) 
VALUES 
    (1000, 'admin', '21232f297a57a5a743894a0e4a801fc3', '管理员', 10, 5000, 10000),
    (1001, 'test', 'cc03e747a6afbbcbf8be7668acfebee5', '测试用户', 5, 2500, 5000);

-- 创建会话表（可选，用于跟踪在线用户）
CREATE TABLE IF NOT EXISTS player_sessions (
    session_id VARCHAR(100) PRIMARY KEY,
    role_id INTEGER NOT NULL,
    username VARCHAR(50) NOT NULL,
    login_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expire_time TIMESTAMP NOT NULL,
    ip_address VARCHAR(45),
    user_agent TEXT,
    FOREIGN KEY (role_id) REFERENCES players(role_id)
);

-- 创建会话索引
CREATE INDEX IF NOT EXISTS idx_sessions_role_id ON player_sessions(role_id);
CREATE INDEX IF NOT EXISTS idx_sessions_expire_time ON player_sessions(expire_time); 