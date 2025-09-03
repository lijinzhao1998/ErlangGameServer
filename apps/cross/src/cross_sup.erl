%% Top-level supervisor for cross service (starts DynamicSupervisor directly)
-module(cross_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% 启动日志系统
    Logger = {logger, {logger, start_link, []}, permanent, 5000, worker, [logger]},
    
    %% 启动ID生成器
    IdGenerator = {id_generator, {id_generator, start_link, []}, permanent, 5000, worker, [id_generator]},
    
    %% 启动MDB数据库
    Mdb = {mdb, {mdb, start_link, []}, permanent, 5000, worker, [mdb]},
    
    %% 启动玩家认证模块
    PlayerAuth = {player_auth, {player_auth, start_link, []}, permanent, 5000, worker, [player_auth]},
    
    %% 启动角色定时器模块
    RoleTimer = {role_timer, {role_timer, start_link, []}, permanent, 5000, worker, [role_timer]},
    
    %% 启动服务器配置模块
    ServerConfig = {server_config, {server_config, start_link, []}, permanent, 5000, worker, [server_config]},
    
    %% 启动时间刷新管理模块
    TimeRefreshManager = {time_refresh_manager, {time_refresh_manager, start_link, []}, permanent, 5000, worker, [time_refresh_manager]},
    
    %% Start a DynamicSupervisor directly as a supervised child (no wrapper).
    %% The DynamicSupervisor will be registered locally as 'dynamic_conn_sup'.
    DynChild = {dynamic_conn_sup,
                {dynamic_supervisor, start_link, [{local, dynamic_conn_sup}, {one_for_one, 20, 60}]},
                permanent, 2000, supervisor, [dynamic_supervisor]},

    Listener = {cross_listener, {cross_listener, start_link, [6000]}, permanent, 5000, worker, [cross_listener]},

    {ok, {{one_for_one, 10, 60}, [Logger, IdGenerator, Mdb, PlayerAuth, RoleTimer, ServerConfig, TimeRefreshManager, DynChild, Listener]}}.