-module(auth_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beehive.hrl").

setup() ->
  bh_test_util:setup(),
  ok.

teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
   {setup,
    fun setup/0,
    fun teardown/1,
    [
     fun test_get_authorized_user/0,
     fun test_run_if_authorized/0,
     fun test_run_if_admin/0
    ]
   }
  }.

test_get_authorized_user() ->
  User = bh_test_util:dummy_user(),
  FoundUser = auth_utils:get_authorized_user([{token, User#user.token}]),
  ?assertEqual(User, FoundUser),

  {error, 401, _} = auth_utils:get_authorized_user([{token, "badtoken"}]),
  {error, 401, _} = auth_utils:get_authorized_user([{token, ""}]),
  {error, 401, _} = auth_utils:get_authorized_user([]),

  passed.

test_run_if_authorized() ->
  User = bh_test_util:dummy_user(),
  Email = auth_utils:run_if_authorized(fun(U) -> U#user.email end,
                                       [User#user.email],
                                       [{token, User#user.token}]),
  ?assertEqual(User#user.email, Email),

  %% Error cases
  {error, 401, _} =
    auth_utils:run_if_authorized(fun(U) -> U#user.email end,
                                 [],
                                 [{token, User#user.token}]),
  {error, 401, _} =
    auth_utils:run_if_authorized(fun(U) -> U#user.email end,
                                 [User#user.email],
                                 [{token, ""}]),
  passed.

test_run_if_admin() ->
  Admin = bh_test_util:admin_user(),
  Email = auth_utils:run_if_admin(fun(U) -> U#user.email end,
                                       [{token, Admin#user.token}]),
  ?assertEqual(Admin#user.email, Email),

  %% Error cases
  User = bh_test_util:dummy_user(),
  {error, 401, _} =
    auth_utils:run_if_admin(fun(U) -> U#user.email end,
                                 [{token, User#user.token}]),
  {error, 401, _} =
    auth_utils:run_if_admin(fun(U) -> U#user.email end,
                                 [{token, ""}]),
  passed.
