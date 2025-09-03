-- 创建物品表
CREATE TABLE IF NOT EXISTS player_items (
    item_id INTEGER PRIMARY KEY,
    role_id INTEGER NOT NULL,
    item_type VARCHAR(50) NOT NULL,
    item_name VARCHAR(100) NOT NULL,
    quantity INTEGER DEFAULT 1,
    quality INTEGER DEFAULT 1,
    level INTEGER DEFAULT 1,
    durability INTEGER DEFAULT 100,
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_use_time TIMESTAMP,
    FOREIGN KEY (role_id) REFERENCES players(role_id)
);

-- 创建物品索引
CREATE INDEX IF NOT EXISTS idx_items_role_id ON player_items(role_id);
CREATE INDEX IF NOT EXISTS idx_items_type ON player_items(item_type);
CREATE INDEX IF NOT EXISTS idx_items_name ON player_items(item_name);

-- 创建公会表
CREATE TABLE IF NOT EXISTS guilds (
    guild_id INTEGER PRIMARY KEY,
    guild_name VARCHAR(100) UNIQUE NOT NULL,
    leader_id INTEGER NOT NULL,
    member_count INTEGER DEFAULT 1,
    max_members INTEGER DEFAULT 100,
    level INTEGER DEFAULT 1,
    exp INTEGER DEFAULT 0,
    funds INTEGER DEFAULT 0,
    description TEXT,
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_active_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    status INTEGER DEFAULT 1,
    FOREIGN KEY (leader_id) REFERENCES players(role_id)
);

-- 创建公会索引
CREATE INDEX IF NOT EXISTS idx_guilds_name ON guilds(guild_name);
CREATE INDEX IF NOT EXISTS idx_guilds_leader ON guilds(leader_id);
CREATE INDEX IF NOT EXISTS idx_guilds_status ON guilds(status);

-- 创建公会成员表
CREATE TABLE IF NOT EXISTS guild_members (
    guild_id INTEGER NOT NULL,
    role_id INTEGER NOT NULL,
    join_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    position VARCHAR(50) DEFAULT 'member',
    contribution INTEGER DEFAULT 0,
    last_contribution_time TIMESTAMP,
    PRIMARY KEY (guild_id, role_id),
    FOREIGN KEY (guild_id) REFERENCES guilds(guild_id),
    FOREIGN KEY (role_id) REFERENCES players(role_id)
);

-- 创建公会成员索引
CREATE INDEX IF NOT EXISTS idx_guild_members_guild ON guild_members(guild_id);
CREATE INDEX IF NOT EXISTS idx_guild_members_role ON guild_members(role_id);

-- 插入测试数据
INSERT OR IGNORE INTO player_items (item_id, role_id, item_type, item_name, quantity, quality, level) 
VALUES 
    (20001, 1000, 'weapon', '铁剑', 1, 2, 5),
    (20002, 1000, 'armor', '皮甲', 1, 1, 3),
    (20003, 1001, 'weapon', '木弓', 1, 1, 2),
    (20004, 1001, 'consumable', '生命药水', 10, 1, 1);

-- 插入测试公会
INSERT OR IGNORE INTO guilds (guild_id, guild_name, leader_id, member_count, max_members, level, exp, funds, description) 
VALUES 
    (40001, '王者公会', 1000, 2, 100, 5, 5000, 10000, '最强公会，欢迎加入！'),
    (40002, '新手村', 1001, 1, 50, 2, 1000, 2000, '新手友好公会');

-- 插入公会成员
INSERT OR IGNORE INTO guild_members (guild_id, role_id, position, contribution) 
VALUES 
    (40001, 1000, 'leader', 1000),
    (40001, 1001, 'member', 500),
    (40002, 1001, 'leader', 200); 