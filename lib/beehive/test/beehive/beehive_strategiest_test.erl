-module (beehive_strategiest_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun random/0,
        fun least_loaded/0
      ]
    }
  }.

random() ->  
  List = [1,2,3,4,5,b,c,d,a],
  Out = list_head(List),
  ?assert(is_list_item(Out, List)),
  passed.

list_head(List) when is_list(List) ->
  erlang:hd(List).
  
is_list_item(Item, List) when is_list(List) ->
  lists:member(Out, List).
  
least_loaded() ->
  List = [
    #bee{id={"chacha", {127,0,0,1}, 9001}},
    #bee{id={"meringue", {127,0,0,1}, 9002}},
    #bee{id={"keylime", {127,0,0,1}, 9003}}
  ],
  Out = list_head(bee_strategies:least_loaded(List)),
  ?assert(is_list_item(Out, List)),
  passed.
