%%%-------------------------------------------------------------------
%%% File    : socket_server_sup.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Fri Nov  6 10:33:51 PST 2009
%%%-------------------------------------------------------------------

-module (tcp_socket_server_sup).

-behaviour(supervisor).

-export([
  start_client/1,
  start_link/0,
  init/1,
  stop/1
]).


-define (MaxRestartTrial, 5).
-define (MaxTimeBetweenRestartInSec, 10).
-define (TcpTimeoutInSec, 2000).
-define(is_simple(Strategy), Strategy =:= simple_one_for_one).
-define(is_one_for_one(Strategy), Strategy =:= one_for_one).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Args) ->
  supervisor:start_child(the_proxy_srv, [Args]).

init([]) ->
  WorkerSpecSet =  list:flattern([
	tcp_socket_server_spec(),
	proxy_server_spec()
	]),
  {ok, {worker_restart_strategy('one_for_one'), 
	WorkerSpecSet}};

init([Module]) ->
  ProxySrv = {undefined,{Module,start_link,[]},temporary,2000,worker,[]},
  {ok, {worker_restart_strategy('simple_one_for_one'), [ProxySrv]}}.

stop(_Args) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
-spec worker_restart_strategy(supervisor:strategy())->{supervisor:strategy(), pos_integer(), pos_integer()}.
%Support only 2 restart types
worker_restart_strategy(RestartType) when ?is_simple(RestartType), ?is_one_for_one(RestartType)->
  {RestartType, ?MaxRestartTrial, ?MaxTimeBetweenRestartInSec};
worker_restart_strategy(UnknownRestartType) -> exit ({unsupported_restart_type, UnknownRestartType}).

tcp_socket_server_spec()->
  {the_tcp_socket_server,
   {tcp_socket_server, start_link, []},
    permanent,
    ?TcpTimeoutInSec,
    worker,
    [tcp_socket_server]}.

proxy_server_spec()->
  {the_proxy_srv,
   {supervisor,start_link,[{local, the_proxy_srv},
    ?MODULE, [proxy_handler]]},
    permanent,
    infinity,
    supervisor,
    []}.


