-module('beehive-rpc').
-export([restart_app/1]).

-spec restart_app([NodeString::list(), App::application:application()] -> {'EXIT', complete}.
restart_app([NodeString, App]) when is_list(NodeString), NodeString =/=[] , is_atom(App) ->
  Node = list_to_atom(NodeString),
  io:fwrite("Restarting ~s on ~s...~n~n", [App, Node]),

  pong = net_adm:ping(Node),
  Resp = rpc:call(Node, app_manager, restart_by_name, [App]),
  erlang:display(Resp),

  exit(complete).
