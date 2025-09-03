## 3.1 Login/Gateway 节点（应用名：`game_login`）

```
game_login_app
└─ game_login_sup (one_for_one)
   ├─ ws_listener (ranch listener / cowboy) —— 监听/握手
   ├─ session_sup (simple_one_for_one) —— 每连接一个 session_proc
   ├─ auth_srv (gen_server) —— 账号校验、JWT
   └─ router_srv (gen_server) —— 读写 realm 注册表，路由玩家
```

## 3.2 跨服节点（应用名：`game_realm`）

```
game_realm_app
└─ game_realm_sup
   ├─ registry_srv (gen_server) —— 维护 {PlayerId -> ShardPid/Node}
   ├─ hash_ring_srv (gen_server) —— 一致性哈希环，管理 shard 槽位
   ├─ shard_watcher (gen_server) —— 订阅 shard 心跳、上下线
   └─ x_feature_* (gen_server) —— 跨服玩法（聊天/匹配/拍卖等）
```

## 3.3 游戏服节点（应用名：`game_shard`）

```
game_shard_app
└─ game_shard_sup
   ├─ player_sup (simple_one_for_one) —— 每玩家一个 player_proc（短会话数据）
   ├─ world_srv (gen_server) —— 世界/场景入口
   ├─ combat_srv (gen_server) —— 战斗调度
   ├─ rank_srv (gen_server) —— 排行
   └─ persist_srv (gen_server) —— 异步落库/快照
```