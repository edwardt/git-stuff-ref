%%%-------------------------------------------------------------------
%%% File    : app_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Nov 13 11:43:43 PST 2009
%%%-------------------------------------------------------------------

-module (app_controller).
-include ("router.hrl").
-include ("http.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get(["all"]) -> 
  All = app:all(),
  {struct, [{
    "apps",
    lists:map(fun(A) ->
      {struct, ?BINIFY([
        {"name", A#app.name},
        {"url", A#app.url},
        {"last_updated", A#app.updated_at}
      ])}
    end, All)
  }]};

get(_) -> "hello world".

post(["new"], Data) ->
  case app:create(Data) of
    ok -> "Added new app";
    _ -> "There was an error adding backend\n"
  end;
  
post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".