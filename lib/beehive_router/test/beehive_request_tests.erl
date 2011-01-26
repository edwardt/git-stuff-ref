-module (beehive_request_tests).
-ifdef (TEST).
-ifdef(EUNIT).
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
        fun test_dummy/0
      ]
    }
  }.

test_dummy() ->
  passed.
-endif.
-endif.
