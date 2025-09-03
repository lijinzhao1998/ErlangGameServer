-module(server_config).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([get_config/1, set_config/2, get_all_config/0]).
-export([get_server_start_time/0, get_server_days/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CONFIG_FILE, "config/server.conf").
-define(DEFAULT_CONFIG, #{
    server_ip => "127.0.0.1",
    server_port => 8080,
    server_name => "ErlangGameServer",
    server_start_time => {{2024, 1, 1}, {0, 0, 0}}, % 默认开服时间
    max_players => 1000,
    debug_mode => true,
    log_level => info
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% 获取配置项
get_config(Key) ->
    gen_server:call(?MODULE, {get_config, Key}).

%% 设置配置项
set_config(Key, Value) ->
    gen_server:call(?MODULE, {set_config, Key, Value}).

%% 获取所有配置
get_all_config() ->
    gen_server:call(?MODULE, get_all_config).

%% 获取开服时间
get_server_start_time() ->
    get_config(server_start_time).

%% 获取开服天数
get_server_days() ->
    StartTime = get_server_start_time(),
    Now = calendar:universal_time(),
    calendar:time_difference(StartTime, Now).

%% gen_server callbacks
init([]) ->
    Config = load_config(),
    {ok, Config}.

handle_call({get_config, Key}, _From, State) ->
    Value = maps:get(Key, State, undefined),
    {reply, Value, State};

handle_call({set_config, Key, Value}, _From, State) ->
    NewState = State#{Key => Value},
    save_config(NewState),
    {reply, ok, NewState};

handle_call(get_all_config, _From, State) ->
    {reply, State, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 内部函数
load_config() ->
    case file:consult(?CONFIG_FILE) of
        {ok, [ConfigList]} ->
            ConfigMap = maps:from_list(ConfigList),
            maps:merge(?DEFAULT_CONFIG, ConfigMap);
        {error, _} ->
            logger:log_warning("无法加载配置文件 ~s，使用默认配置", [?CONFIG_FILE]),
            ?DEFAULT_CONFIG
    end.

save_config(Config) ->
    ConfigList = maps:to_list(Config),
    case file:write_file(?CONFIG_FILE, io_lib:fwrite("~p.~n", [ConfigList])) of
        ok ->
            logger:log_info("配置已保存到 ~s", [?CONFIG_FILE]);
        {error, Reason} ->
            logger:log_error("保存配置失败: ~p", [Reason])
    end. 