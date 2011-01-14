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

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define (GetValOrDefault (Bool, A, DefaultValue), if Bool -> A; true -> DefaultValue end).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
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
init(_Args) ->

%TODO Should be a different management module%%
%Router shall focus on one responsibility, that is to route to active workers
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

-spec optional_dashboard_childspec()-> list() | [].	
optional_dashboard_childspec()->
  Dashboard = ?CHILD(beehive_dashboard_sup, worker),
  ShouldRunDashboard = should_run_dashboard(),
  ?GetValOrDefault(ShouldRunDashboard, Dashboard, []).

-spec should_run_dashboard() -> {dashboard, boolean()}.
should_run_dashboard()->
  config:search_for_application_value(dashboard, true).

-spec tcp_socket_server_sup_spec()->tuple().
tcp_socket_server_sup_spec()->
  get_worker_childspec(tcp_socket_server_sup).

-spec node_stat_server_spec()-> tuple().
node_stat_server_spec()->
  get_worker_childspec(bh_node_stats_srv).

-spec perfcounter_server_spec() -> tuple().
perfcounter_server_spec()->
  get_worker_childspec(bh_perf).	

-spec get_worker_childspec(application:application()) -> tuple().
get_worker_childspec(Name) when is_atom(Name) ->
  ?CHILD(Name, worker).

-spec worker_restart_strategy() -> tuple().
worker_restart_strategy() ->
  MaxRestartTrial = 5, MaxTimeBetweenRestartInSec =10,
  {one_for_one, MaxRestartTrial, MaxTimeBetweenRestartInSec}.


%%====================================================================
%% Unit test
%%====================================================================
%-ifdef(ETEST)
%-define().





%-endif

	
