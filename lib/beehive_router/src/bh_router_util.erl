-module(bh_router_util).
-include ("../include/router.hrl").

-export([notify_manager/1]).

-export([ensure_loaded/1, ensure_deps_loaded/1]).

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
-spec ensure_loaded(App::application:application()) -> {ok, app_loaded} | {error, term()}.
ensure_loaded(App) when is_atom(App) ->
  case application:loaded(App) of
       ok -> {ok, app_loaded};
       {error, {already_loaded, _Name}} -> {ok, app_loaded};
       Error -> throw({error_load_app, Error})
  end.
	
-spec ensure_started(App::application:application()) -> {ok, app_started} | {error, term()}.	
ensure_started(App) when is_atom(App) ->
  case application:start(App) of
       {ok, _Reason } -> {ok, app_started};
       {error , {already_started, App}} -> ok;
       {error, Error} -> throw({error_start_app, Error})
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
-spec time_diff(ThisTime::non_neg_integer(), ThatTime::non_neg_integer()) -> integer().
time_diff(ThisTimeInSec, ThatTimeInSec) -> 
   ThisTimeInSec - ThatTimeInSec.	

% TODO: seperate this out to another util, see above
%%%%%%%%%%%% File/Folder Util %%%%%%%%%%%%%
%-spec ensure_file(FileName::nonempty_string()) -> 'ok' | {error, term()}.
%ensure_file(FileName)
	

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
   _ -> a
  catch
    _:_ -> io:format("StackTrace is ~p ~n", [erlang:get_stacktrace()])
  end.	
