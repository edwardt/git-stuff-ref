-module(bh_router_util).

-export([notify_manager/1]).

-export([ensure_loaded/1, ensure_deps_loaded/1]).

-export([compare/2,equal/2]).

-export([current_fun/0, get_stacktrace/0]).

-define(Function, hd(element(2,element(2,catch erlang:error([]))))).


notify_manager(Event)-> node_manager:notify(Event).

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
		{ok, _Reason } -> ok;
		{error , {already_started, App} -> ok;
		{error, Error} -> throw({error_start_app, Error})
	end. 
			
ensure_deps_loaded([])-> {ok, app_loaded};

ensure_deps_loaded(Apps) when is_list(Apps)->
	list:map( fun(App)-> ensure_loaded(App)end, Apps).	
	
%%%%%%%%%% Data type Util %%%%%%%%%%%%%%%%% 
-spec compare(A::term(), B::term()) -> 'less' | 'equal' | 'greater'.
compare(A,B) when A<B -> less;
compare(A, B) when A == B -> equal;
compare(A,B) when A>B -> greater.

-spec equal(Same::term(),Same::term()) -> 'true' | 'false'.
equal(Same, Same) -> true;
equal(_Other, _Other) -> false.

%%%%%%%%%%%% Date Time Util %%%%%%%%%%%%%%%% 
-spec time_diff(ThisTime::non_neg_integer(), ThatTime::non_neg_integer()) -> integer().
time_diff(ThisTimeInSec, ThatTimeInSec) -> 
   ThisTimeInSec - ThatTimeInSec.	

%%%%%%%%%%%% File/Folder Util %%%%%%%%%%%%%
%-spec ensure_file(FileName::nonempty_string()) -> 'ok' | {error, term()}.
%ensure_file(FileName)
	

%%%%%%%%%% Config util %%%%%%%%%%%%%%%%%%%%

	
%%%%%% Print %%%%%%%%%%%%%%%%%
format(Mod, Func, Line, Why) ->
  io:format("Mod: ~w Func: ~w Line: ~w, Why: ~p",[Mod, Func, Line, Why]).

format(Where, Why)->
  io:format("Where: ~p Reason: ~p",[Where, Why]).
		
error_msg(Mod, Func, Line, Why) ->
  error_logger:error_msg(format(Mod, Func, Line, Why)).	
    	
info_msg(Where, Why, StackTrace) when is_function(Where, 0), 
				 is_function(StackTrace, 0) ->
  error_logger:info_msg(format(Where, Why)).	

warning_msg(Where, Why, StackTrace) when is_function(Where, 0),
				 is_function(StackTrace, 0) ->
  error_logger:warning_msg(format(Where, Why)).
  	
error_msg(Where, Why, StackTrace) when is_function(Where, 0),
				 is_function(StackTrace, 0) ->
  error_logger:error_msg(format(Where, Why)).

current_fun()-> {current_function, ?Function}.
  
get_stacktrace() ->
  try throw(a) of
   _ -> a
  catch
    _:_ -> io:format("StackTrace is ~p ~n", [erlang:get_stacktrace()])
  end.	
