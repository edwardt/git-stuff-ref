%%%-------------------------------------------------------------------
%%% File    : bh_node_stats_srv.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Sun Dec 13 20:51:11 PST 2009
%%%-------------------------------------------------------------------

-module (bh_node_stats_srv).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  node_stat/1,
  node_dump/0,
  node_dump/1,
  node_dump/2,
  get_cpu_load/0,
  get_avg_cpu_load/0,
  get_free_memory/0,
  get_net_stat/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  node_stats   % dict of the node stats
}).

-define(PROCNET, "/proc/net/dev").
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
node_stat({node_stat, Key, Value, Time})        ->
  gen_server:cast(?SERVER, {node_stat, Key, Value, Time}).

node_dump() -> gen_server:call(?SERVER, {node_dump}).
node_dump(Key) -> gen_server:call(?SERVER, {node_dump, Key}).
node_dump(Key, Range) -> gen_server:call(?SERVER, {node_dump, Key, Range}).

get_cpu_load()-> gen_server:call(?SERVER,{get_cpu_load}).
get_avg_cpu_load()-> gen_server:call(?SERVER, {get_avg_cpu_load}).
get_free_memory() -> gen_server:call(?SERVER, {get_free_mem}).
get_net_stat() -> gen_server:call(?SERVER,{get_net_stat}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  io:format("Starting bh_node_stats_srv~n"),
  process_flag(trap_exit, true),
  %ensure_app_started(os_mon), 
  ensure_apps_started(),
  State = #state{
    node_stats  = dict:new()
  },

  {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({node_dump}, _From, #state{node_stats = Dict} = State) ->
  {reply, dict:to_list(Dict), State};

handle_call({node_dump, Key}, _From, #state{node_stats = Dict} = State) ->
  Reply = case dict:find(Key, Dict) of
    error -> [];
    {ok, E} -> E
  end,
  {reply, Reply, State};

handle_call({node_dump, Key, Range},
            _From, #state{node_stats = Dict} = State) ->
  StatsList = case dict:find(Key, Dict) of
    error -> [];
    {ok, E} -> E
  end,
  Reply = lists:sublist(StatsList, Range),
  {reply, Reply, State};
  
handle_call({get_cpu_load}, _From, State) ->
  Reply = [cpu_load, get_os_data(cpu)],
  {reply, Reply, State};
  
handle_call({get_avg_cpu_load}, _From, State) ->  
  Reply = [avg_cpu_load, get_os_data(cpu_load)],
  {reply, Reply, State};
  
handle_call({get_free_mem}, _From, State) ->
  Reply = [free_mem_byte, get_os_data(free_mem)],
  {reply, Reply, State};

handle_call({get_net_stat}, _From, State) ->
  Reply = [packets, get_os_data(packets)],
  {reply, Reply, State};  

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({node_stat, Key, Value, Time}, #state{node_stats = Dict} = State) ->
  NewDict = case dict:find(Key, Dict) of
    error -> dict:store(Key, [{Time, Value}], Dict);
    {ok, CurrentVal} -> dict:store(Key, [{Time, Value}|CurrentVal], Dict)
  end,
  {noreply, State#state{node_stats = NewDict}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
  
%%====================================================================
%% Internal functions
%%====================================================================
-spec ensure_apps_started()-> no_return() | {error, term()}.
ensure_apps_started()->  
  bh_router_util:ensure_deps_started(['sasl','os_mon']).
  
  
%% Return node cpu utilisation
get_os_data(cpu) -> cpu_sup:util();
%% Return node cpu average load on 1 minute;
get_os_data(cpu_load) -> cpu_sup:avg1()/256;
get_os_data(DataName) -> get_os_data(DataName,os:type()).
%%--------------------------------------------------------------------
%% Func: get_os_data/2
%%--------------------------------------------------------------------
%% Return free memory in bytes.
%% Use the result of the free commands on Linux and os_mon on all
%% other platforms
get_os_data(freemem, {unix, linux}) ->
    Result = os:cmd("free | grep '\\-/\\+'"),
    [_, _, _, Free] = string:tokens(Result, " \n"),
    list_to_integer(Free)/1024;
get_os_data(freemem, {unix, sunos}) ->
    Result = os:cmd("vmstat 1 2 | tail -1"),
    [_, _, _, _, Free | _] = string:tokens(Result, " "),
    list_to_integer(Free)/1024;

get_os_data(freemem, _OS) ->
    Data = memsup:get_system_memory_data(),
    {value,{free_memory,FreeMem}} = lists:keysearch(free_memory, 1, Data),
    %% Megabytes
    FreeMem/1048576;

%% Return packets sent/received on network interface
get_os_data(packets, {unix, linux}) ->
    get_os_data(packets, {unix, linux},?PROCNET);
%% solaris
get_os_data(packets, {unix, sunos}) ->
    Result = os:cmd("netstat -in 1 1 | tail -1"),
    [_, _, _, _, _, RecvPackets, _, SentPackets | _] = string:tokens(Result, " "),
    {list_to_integer(RecvPackets), list_to_integer(SentPackets)};
get_os_data(packets, _OS) ->
    {0, 0 }. % FIXME: not implemented for other arch.

%% packets Linux, special case with File as a variable to easy testing
get_os_data(packets, {unix, linux},File) ->
    {ok, Lines} = ts_utils:file_to_list(File),
    %% get the cumulative traffic of all ethX interfaces
    Eth=[io_lib:fread("~d~d~d~d~d~d~d~d~d~d", X) ||
        {E,X}<-lists:map(fun(Y)->ts_utils:split2(Y,$:,strip) end ,Lines),
        string:str(E,"eth") /= 0],
    Fun = fun (A, {Rcv, Sent}) ->
                  {ok,[_RcvBytes,RcvPkt,_,_,_,_,_,_,_SentBytes,SentPkt],_}=A,
                  {Rcv+RcvPkt,Sent+SentPkt}
          end,
    lists:foldl(Fun, {0,0}, Eth).
    

%%--------------------------------------------------------------------
%% Unit Test 
%%--------------------------------------------------------------------    
-ifdef(TEST).
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

ensure_apps_started_test()->
  Result = ensure_apps_started().
  ?assertMatch({ok, _Whatever}, Result).

get_os_data_platform_linux_test()->
 %BUILD
   
 %DO
 
 %ASSERT
  ok.
 
get_os_data_platform_unknown_test()->
 % Result = get_os_data(
  ok.

get_packets_test()_>

  ok.

get_free_mem_test()->

  ok.
  
get_cpu_load_test()->

  ok.

get_cpu_utilization_test()->
 
  ok.

-endif.    
-endif.
    
