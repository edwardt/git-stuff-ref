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
-define (IF (Bool, A, B), if Bool -> A; true -> B end).

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
  ChildrenSpec = lists:flatten([
    get_worker_process_runspec(tcp_socket_server_sup), 
    get_worker_process_runspec(bh_node_stats_srv),
    get_worker_process_runspec(bh_perf),
    get_run_dashboard_runspec()
   ]),
  {ok,get_worker_restartstrategy(ChildrenSpec)}.


%%====================================================================
%% Internal functions
%%====================================================================

-spec get_run_dashboard_runspec()-> list() | [].	
get_run_dashboard_runspec()->
  Dashboard = ?CHILD(beehive_dashboard_sup, worker),
  ShouldRunDashboard = should_run_dashboard(),
  ?IF(ShouldRunDashboard, Dashboard, []).

-spec should_run_dashboard() -> {dashboard, boolean()}.
should_run_dashboard()->
  config:search_for_application_value(dashboard, true).

-spec get_worker_process_runspec(application:application()) -> list().
get_worker_process_runspec(Name) when is_atom(Name) ->
  ?CHILD(Name, worker).

-spec get_worker_restartstrategy(list()) -> tuple().
get_worker_restartstrategy(WorkerProcessSpec) when is_list(WorkerProcessSpec), WorkerProcessSpec =/= []->
  MaxRestartTrial = 5, MaxTimeBetweenRestartInSec =10,
  {ok, {{one_for_one, MaxRestartTrial, MaxTimeBetweenRestartInSec}, WorkerProcessSpec}}.
	
