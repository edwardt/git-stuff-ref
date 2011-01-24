%%%-------------------------------------------------------------------
%%% File    : bh_router_sup.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Wed Dec  2 20:13:46 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_router_sup).

-behaviour(supervisor).
-compile([verbose, report_errors, report_warnings, trace, debug_info]).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define (MaxRestartTrial, 5).
-define (MaxTimeBetweenRestartInSec, 10).
-define (ShutdownAfterTimeoutInSec, 5000).
%-define ().
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define (GetValOrDefault (Bool, A, DefaultValue), if Bool -> A; true -> DefaultValue end).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link()-> supervisor:startlink_ret() | supervisor:startlink_err().
start_link() ->
  start_link([]).
start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
-spec init(list()) -> {'ok', supervisor:state()} | 'ignore' | {'stop', supervisor:stop_rsn()}.
init(_Args) ->
%TODO Should be a different management module%%
%Router shall focus on one responsibility, ie. routing to active workers
  ChildSpecSet = lists:flatten([
    tcp_socket_server_sup_spec(), 
    node_stat_server_spec(),
    perfcounter_server_spec(),
    optional_dashboard_childspec()
   ]),
  {ok, worker_restart_strategy(), ChildSpecSet}.
  

%%====================================================================
%% Internal functions
%%====================================================================

-spec optional_dashboard_childspec()-> supervisor:childspec() | [].  	
optional_dashboard_childspec()->
  Dashboard = get_worker_childspec(beehive_dashboard_sup),
  ShouldRunDashboard = should_run_dashboard(),
  ?GetValOrDefault(ShouldRunDashboard, Dashboard, []).

-spec should_run_dashboard() -> {dashboard, boolean()}.
should_run_dashboard()->
  config:search_for_application_value(dashboard, true). %TODO: That should not bee TOOO many ways to do config. 
							%TODO: Invitation to user error!! Reject being 'cute'.

-spec tcp_socket_server_sup_spec()->supervisor:childspec().
tcp_socket_server_sup_spec()->
  get_worker_childspec(tcp_socket_server_sup).

-spec node_stat_server_spec()-> supervisor:childspec().
node_stat_server_spec()->
  get_worker_childspec(bh_node_stats_srv).

-spec perfcounter_server_spec() -> supervisor:childspec().
perfcounter_server_spec()->
  get_worker_childspec(bh_perf).	

-spec get_worker_childspec(application:application()) -> supervisor:childspec().
get_worker_childspec(Name) when is_atom(Name) ->
  {Name, {Name, start_link, []}, permanent, ?ShutdownAfterTimeoutInSec, worker, [Name]}.

-spec worker_restart_strategy() -> {supervisor:strategy(), pos_integer(), pos_integer()}.
worker_restart_strategy() ->
  {one_for_one, ?MaxRestartTrial, ?MaxTimeBetweenRestartInSec}.

-spec get_app_env(Key::atom(), Default::term())-> term(). 
get_app_env(Key, Default) ->
  get_app_env('beehive', Key, Default).	
  	  
-spec get_app_env(App::application:application(), 
		  Key::atom(), 
		  Default::any()) -> term().		  
get_app_env(App, Key, Default)->
  case application:get_env(application:get_application(App), Key) of
       {ok, Val} -> Val;
  	_Other ->
		case init:get_argument(Key) of
       		   [[Val | _]] -> Val;
                   error -> Default
                end
  end.

  

%%====================================================================
%% Unit test
%%====================================================================
-ifdef(TEST).
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

should_run_dashboard_test_()->
  ?assertTrue(should_run_dashboard()).

get_worker_childspec_test_()->
   Name = testApp, 
   ensure_app_env_absent(Name).
   Expected = {Name, {Name, start_link, []}, permanent, ?ShutdownAfterTimeoutInSec, worker, [Name]}.
   ?assertMatch(Expected, get_worker_childspec(Name)).
     	
get_worker_childspec_invalid_apptype_test_()->
  ?assertNot(get_worker_childspec("TestApp_As_String").

worker_restart_strategy_test_() ->
  ?assertEqual({one_for_one,?MaxRestartTrial, ?MaxTimeBetweenRestartInSec },
  	 	worker_restart_strategy()). 

get_app_env_test()_->
  App = testApp, Key = testKey,
  ensure_app_env_absent(testApp).
  Default = defaultVal,
  Result = get_app_env(App, Key, Default),
  ?assertEqual(Default,Result),
  ensure_app_env_absent(testApp).
  


ensure_app_env_absent(App, Key) when is_atom(App), is_atom(Key)->
    ok = application:unset_env(App, Key).  
    

-endif.
-endif.

	
