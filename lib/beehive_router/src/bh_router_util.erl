-module(bh_router_util).
-include ("../include/router.hrl").

-export([notify_manager/1]).

-export([ensure_loaded/1, ensure_deps_loaded/1, ensure_deps_started/1]).

-export([compare/2,equal/2]).

-export([warning_msg/4, info_msg/3, 
         error_msg/3, error_msg/4]).
-export([current_fun/0, get_stacktrace/0]).

%TODO move to hrl.
%-define(Function, hd(element(2,element(2,catch erlang:error([]))))).

notify_manager(Event)-> node_manager:notify(Event).

% Hosts higher order of utility functions for router on the 
% application level
%%%% Application Runtime %%%%
-spec ensure_loaded(App::application:application()) -> {ok, 'app_loaded'} | {error, term()} | {'error_load_app', term()}.
ensure_loaded(App) when is_atom(App) ->
  case application:loaded(App) of
       ok -> {ok, app_loaded};
       {error, {already_loaded, _Name}} -> {ok, app_loaded};
       Error -> throw({error_load_app, Error})
  end.
	
-spec ensure_started(App::application:application()) -> {ok, 'app_started'} | {error, term()} | {'error_start_app', term()}.	
ensure_started(App) when is_atom(App) ->
  case application:start(App) of
       {ok, _Reason } -> {ok, app_started};
       {error , {already_started, App}} -> {ok, app_started};
       {error, Error} -> throw({error_start_app, Error})
  end. 

-spec ensure_stopped(App::application:application()) -> {ok, 'app_stopped'} | {error, term()} |{'error_stop_app', term()}.
ensure_stopped(App) when is_atom(App)->
  case application:stop(App) of
 	{ok, _Reason} -> {ok, app_stopped};
	{error, {already_stopped, App}} -> {ok, app_stopped};
	{error, Error} -> throw({error_stop_app, Error})
  end.
		
ensure_deps_loaded([])-> {ok, app_loaded};
ensure_deps_loaded(Apps) when is_list(Apps)->
  list:map( fun(App)-> ensure_loaded(App)end, Apps).
	
ensure_deps_started([]) ->{ok, apps_started};
ensure_deps_started(Apps) when is_list(Apps) ->
  list:map(fun(App) -> ensure_started(App) end, Apps).	

% TODO: seperate this out to another util file, each util file provides util for 1 
% group of concept
	
%%%%%%%%%% Data type Util  %%%%%%%%%%%%%%%%% 
-spec compare(A::term(), B::term()) -> 'less' | 'equal' | 'greater'.
compare(A,B) when A<B -> less;
compare(A, B) when A == B -> equal;
compare(A,B) when A>B -> greater.

-spec equal(Same::term(),Same::term()) -> 'true' | 'false'.
equal(Same, Same) -> true;
equal(_Other, _Other) -> false.



% TODO: seperate this out to another util, see above
%%%%%%%%%%%% Date Time Util %%%%%%%%%%%%%%%% 
%TODO: this is not a good func for time diff, just pull this out from dup code from product
-spec time_diff(ThisTime::non_neg_integer(), ThatTime::non_neg_integer()) -> integer().

time_diff(Now, Then) -> 
   time_diff(Now,Then, 0).
time_diff(Now, Then, 0) ->
   Val = (Now - Then),
   if(Val < 0)-> Val;
      true -> 0      
   end.
    	     	

% TODO: seperate this out to another util, see above
%%%%%%%%%%%% File/Folder Util %%%%%%%%%%%%%
%-spec ensure_file(FileName::nonempty_string()) -> 'ok' | {error, term()}.
%ensure_file(FileName)


% TODO: seperate this out to another util, see above
%%%%%%%%%%%% HTTP Util %%%%%%%%%%%%%
to_json(Data) when is_list(Data)->
  mochijson2:encode(Data).
	

% TODO: seperate this out to another util, see above	
%%%%%% Print Util%%%%%%%%%%%%%%%%%
format(Mod, Func, Line, Why) ->
  io:format("Mod: ~w Func: ~w Line: ~w, Why: ~p",[Mod, Func, Line, Why]).
format(Mod, Func, Why) ->
  io:format("Mod: ~w Func: ~w Why: ~p",[Mod, Func, Why]).
format(Where, Why)->
  io:format("Where: ~p Reason: ~p",[Where, Why]).
		
debug_msg(Where, Why, StackTrace) when is_function(Where,0),
				  is_function(StackTrace) ->
  info_msg(Where, Why, StackTrace).				  	
debug_msg(Mod, Func, Line, What)->
  error_logger:info_msg(format(Mod, Func, Line, What)).
 
%info_msg(Mod, Func, Line, What) ->
%  error_logger:info_msg(format(Mod, Func, Line, What)).    	
%info_msg(Where, Why, StackTrace) when is_function(Where, 0), 
%				 is_function(StackTrace, 0) ->
%  error_logger:info_msg(format(Where, Why, StackTrace)).	
info_msg(Mod, Func, What)->
  error_logger:info_msg(format(Mod, Func, What)).
  
warning_msg(Mod, Func, Line, What)->
  error_logger:warning_msg(format(Mod, Func, Line, What)).
%warning_msg(Where, Why, StackTrace) when is_function(Where, 0),
%				 is_function(StackTrace, 0) ->
%  error_logger:warning_msg(format(Where, Why)).
  
error_msg(Mod, Func, Line, Why) ->
  error_logger:error_msg(format(Mod, Func, Line, Why)).  	
error_msg(Where, Why, StackTrace) when is_function(Where, 0),
				 is_function(StackTrace, 0) ->				  
  error_logger:error_msg(format(Where, Why, StackTrace)).
  

current_fun()-> {current_function, ?Function}.
  
get_stacktrace() ->
  try throw(a) of
   _Hook -> a
  catch
    _Term:_Info -> io:format("StackTrace is ~p ~n", [erlang:get_stacktrace()])
  end.	



%%--------------------------------------------------------------------
%% Unit Test 
%%-------------------------------------------------------------------- 
-ifdef(TEST).
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
ensure_app_new_loaded_test()->
  %Buid,, do, check
  ok.

ensure_app_already_loaded_test()->
  %App start
  %check load
  
  ok.

ensure_app_not_found_loaded_test()->
  %check load
  ok.

ensure_apps_loaded_test()->
  %appstart sasl, os_mon
  %check loaded
  ok.

ensure_apps_one_failed_loaded_test()->
  %appstart sasl, inet, os_mon
  %check loaded
  ok.

ensure_apps_started_emptyset_test()->
  ok.

ensure_apps_started_OneElementSet_test()->
  ok.


ensure_app_new_started_test()->
  ok.

ensure_app_already_started_test()->
  ok.

ensure_app_not_found_started_test()->
  ok.

ensure_apps_stopped_test()->
  ok.
  
ensure_apps_already_stopped_test()->
  ok.  

ensure_apps_stopped_emptyset_test()->
  ok.

ensure_apps_stopped_one_failed_test()->
  %appstart sasl, inet, os_mon
  %check loaded
  ok.

ensure_apps_stopped_OneElementSet_test()->
  ok.

time_diff_test()->
  Then = erlang:now(),
  erlang:sleep(10),
  Now = erlang:now(),
  ?assertMatch(10, time_diff(Then, Now)).

-endif.
-endif.

