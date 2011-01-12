-module(bh_router_util).

-export([notify_manager/1]).

-export([ensure_loaded/1, ensure_deps_loaded/1]).

notify_manager(Event)-> node_manager:notify(Event).



%%%% Application Runtime %%%%
ensure_loaded(App) when is_atom(App) ->
	case application:loaded(App) of
		ok -> {ok, app_loaded};
		{error, {already_loaded, _Name}} -> {ok, app_loaded};
		Error -> {error, Error}
	end.
	
ensure_deps_loaded([])-> ok;

ensure_deps_loaded(Apps) when is_list(Apps)->
	list:map( 
		fun(App)-> 
			case (ensure_loaded(App)) of 
				{ok, app_loaded} -> {ok, app_loaded};
				Error -> error_msg(?MODULE, ensure_deps_loaded, ?LINE, Error)
			end
		
		end, 
		Apps)
	.
	
%%%%%%%%%% Data type Util %%%%%%%%%%%%%%%%%%%%5
compare(A,B) when A<B -> less;
compare(A, B) when A == B -> equal;
compare(A,B) when A>B --> greater.

equal(Same, Same) -> true;
equal(_Other, _Other) -> false.
	
%%%%%% Print %%%%%%%%%%%%%%%%%
%print_list([])->ok;
%print_list(L, Pattern) when is_list(L) ->

format(Mod, Func, Line, Why) ->
	io:format("Mod: ~w Func: ~w Line: ~w, Why: ~p",[Mod, Func, Line, Why]).
		
error_msg(Mod, Func, Line, Why) ->
	error_logger:error_msg(format(Mod, Func, Line, Why)).		
