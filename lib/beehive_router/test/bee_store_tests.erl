-module (bee_store_tests).
%-ifdef (TEST).
%-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ok.
  
teardown(_X) ->
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_startup/0
      ]
    }
  }.

test_startup() ->
  passed.
%-endif.
%-endif.
