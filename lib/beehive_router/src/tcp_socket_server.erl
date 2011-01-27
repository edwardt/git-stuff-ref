%%%-------------------------------------------------------------------
%%% File    : tcp_socket_server.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Fri Nov  6 11:11:36 PST 2009
%%%-------------------------------------------------------------------

-module (tcp_socket_server).

-include ("beehive.hrl").
-include ("http.hrl").
-include ("common.hrl").
-include ("../include/router.hrl").
-include ("../include/bh_stat.hrl").

%% API
-export([
  start_link/0,
  init/0,
  init/1,
  init_accept/1,
  stop/0
]).

-define (Socket_Server_Sup, tcp_socket_server_sup).

%TODO: not a good acceptor/reactor. Also why not use gen_server? 
%TODO: Why give up all the benefits where supervisor will do that for us?
%%====================================================================
%% API
%%====================================================================
% TODO: Move to proc_lib:start_link
start_link()          -> init().
% Start listening on the application port
% this can be modified with a config file or an environment variable 
% e.g. BEEHIVE_CLIENT_PORT=80
init()                ->
  init(get_client_port()).
init(LocalPort) ->
  ensure_environment(),
  Pid = proc_lib:spawn_link(?MODULE, init_accept, [LocalPort]),
  {ok, Pid}.

stop()->
  %Kill all the handlers
  %Stop itself
  Socket = get_client_port(), 
  gen_tcp:close(Socket).

% accept responses on the port given by the application configuration

init_accept(LPort) ->
  OsAcceptQueue = 256,
  SockOpts = [binary, {backlog, OsAcceptQueue}, {nodelay, false},
              {reuseaddr, true},{active, false}],
  init_accept(LPort, SockOpts).

init_accept(LPort, SockOpts) ->
  case gen_tcp:listen(LPort, SockOpts) of
  	{ok, ListenSocket} ->
    			   accept(ListenSocket); %TODO: debug
  	Error ->
	    	?LOG(error,
		 "There was an error listening to the socket for port ~p: ~p",
		 [LPort, Error]),
		 %TODO: what additional info does the above add?
    		 error_msg(current_func(), 
    		 	   io:format("Port: ~p Sock Options: ~p Error: ~p", [LPort, SockOpts, Error]),
    		 	   get_stacktrace()),
    	         %TODO: the server should just die and exit at this point 
    	         exit(socket_listen_error, Error)
    		 %{error, Error}
  end.
  


%% Accept a new socket connection to the server. If the socket
%% connection is successful, then move on and decode the socket type
%% (for now, we'll assume it's an http request) and pass it on to the
%% proxy handler in a separate process. Move on to accept the next
%% request in this process so that we are never blocking the socket
%% server
accept(LSock) ->  
  accept(LSock,'no_debug').

accept(LSock, Debug) ->
    case gen_tcp:accept(LSock) of
    {ok, ClientSock} ->
      spawn(fun() -> pass_on_to_proxy(ClientSock, Debug) end), %TODO: debug
	    accept(LSock);
    Error ->
      %TODO slowly migrate from
      ?LOG(error, "There was an error accepting the socket ~p: ~p",
           [LSock, Error]),
      %The msg above add no additional helpful info     
      error_msg(current_func(), {socket_accept_error, [LSock, Error]}, get_stacktrace()),      
      exit({socket_accept_error, Error})
  end.


%% Take the socket and decode the routing key from the packet. For
%% http, this means accept enough on the request to pull off the
%% headers and decode the 'Host' parameter (or other routing parameter
%% defined by routing_parameter tphen starting a proxy handler proxy
%% process and finally passing the socket to the proxy handler process
pass_on_to_proxy(ClientSock) ->
  pass_on_to_proxy(ClientSock, 'no_debug').
  
pass_on_to_proxy(ClientSock, Debug) ->
  %% Choose here the type of response... for now, it'll just be http,
  %% but in the future... maybe tcp/udp?
  debug_msg(current_func(), "Hand over connection to proxy", Debug),
  {ok, ProxyPid} = ?Socket_Server_Sup:start_client(ClientSock),
  gen_tcp:controlling_process(ClientSock, ProxyPid),
  send_to(ProxyPid, {start, ClientSock, ProxyPid}, Debug).
  %ProxyPid ! {start, ClientSock, ProxyPid}.
  
  
%%%%%%%%%%%%% Internal Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec ensure_environment()-> {ok, all_runtime_deps_ok} | {error, term()}.
ensure_environment()->
 % ok = check port is ok
 % ok = check erlang cookie present not nonode...enforce proper cookie
 % ok = check 
 %ALL the runtime environment should be done
 ok.


-spec get_client_port() -> port() | {error, term()}.
get_client_port()->
  PortNum = get_port(),
  config:search_for_application_value(client_port, PortNum). %TODO: I don't know why we need to hide usage like this

%Note: this is for now. we only support http, prevent people from messing it up for now.
-spec get_port()-> port().
get_port()-> 
  get_port('http-alt'). 
  
-spec get_port(Protocol::protocol()) -> port() | {unsupported_protocol_type, atom()}.

get_port('http-alt') -> 8080; %TODO: get from dets later instead
get_port('https')-> 443;
get_port(UnknownProtocol) -> throw({unsupported_protocol_type, UnknownProtocol}).

%-spec get_socket_sup(protocol())
%get_socket_sup(Protocol)->
 

-spec send_to(To::pid(), {Tag::atom(), Msg::any(), To::pid()}) -> {ok, term()} | 
								  {error, term()}.
 
send_to(To, {Tag, Msg, From}) ->
  To ! {Tag, Msg, From}.
  
-spec send_to(To::pid(), {Tag::atom(), Msg::any(), To::pid()}, 'debug' | any()) -> {ok, term()} | 
								  {error, term()}.
send_to(To, {Tag, Msg, From}, Debug)->
  debug_msg(current_func(),io:format("Message ~w sent to ~w",[To, {Tag, Msg, From}]), Debug), 
  send_to(To, {Tag, Msg, From}).
  
info_msg(Func, What)->
  bh_router_util:info_msg(?MODULE, Func, What).  

%TODO Func may not be needed  
error_msg(Func, Why, StackTrace) ->
  bh_router_util:error_msg(Func, Why, StackTrace).
  
debug_msg(Func, What, 'debug') ->
  info_msg(Func, What);
  
debug_msg(_Func, _What, _ELSE) ->
  ok.

current_func()->
  bh_router_util:current_function().

get_stacktrace()->
  bh_router_util:get_stacktrace().


%%--------------------------------------------------------------------
%% Unit Test Only for internal unexported function
%%--------------------------------------------------------------------  
-ifdef(TEST).
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_client_port_test()->
  TestPort = 100,
  application:unsetenv(client_port),
  ?assertEqual(TestPort, get_client_port()).

get_port_test()->
  ?assertEqual(8080, get_port()).

get_port_unknown_prototcol_test()->
  UnknownProtocol = 'whatever',
  ?assertThrow({unsupported_protocol_type, UnknownProtocol}, 
   get_port(UnknownProtocol)).

debug_msg_unknown_option__ensure_do_nothing_test()->  
  ?assertEqual(ok, debug_msg(somefunc, "some reason", 'whetever')).
  
get_stacktrace_test()->
  ok.

get_error_msg_test()->
  ok.

-endif.
-endif.
 
  

