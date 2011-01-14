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

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Args) ->
  supervisor:start_child(the_proxy_srv, [Args]).

init([]) ->
  WorkerSpecSet =  list:flattern([tcp_socket_server_spec(),proxy_server_spec()]),

  {ok, {worker_restart_strategy(), 
	WorkerSpecSet}};

init([Module]) ->
  ProxySrv = {undefined,{Module,start_link,[]},temporary,2000,worker,[]},
  {ok, {{simple_one_for_one, 5, 10}, [ProxySrv]}}.

stop(_Args) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
-spec worker_restart_strategy()->{supervisor:strategy(), pos_integer(), pos_integer()}.
worker_restart_strategy()->
  {simple_one_for_one, ?MaxRestartTrial, ?MaxTimeBetweenRestartInSec}.

tcp_socket_server_spec()->
  {the_tcp_socket_server,
   {tcp_socket_server, start_link, []},
    permanent,
    2000,
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


