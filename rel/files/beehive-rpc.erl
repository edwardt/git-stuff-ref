-module('beehive-rpc').
-export([restart_app/1]).

restart_app([NodeString, App]) ->
  Node = list_to_atom(NodeString),
  io:fwrite("Restarting ~p on ~p...~n~n", [App, Node]),

  pong = net_adm:ping(Node),
  Resp = rpc:call(Node, app_manager, restart_by_name, [App]),
  erlang:display(Resp),

  exit(complete).
