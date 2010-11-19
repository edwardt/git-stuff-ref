-module(gitolite_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beehive.hrl").
-include("common.hrl").

setup() ->
  bh_test_util:setup(app),
  ok.

teardown(_X) ->
  ok.

starting_test_() ->
  {inorder,
   {setup,
    fun setup/0,
    fun teardown/1,
    [
     fun test_write_pubkey/0,
     fun test_app_config_info/0,
     fun test_write_config_file/0
    ]
   }
  }.

test_write_pubkey() ->
  User = bh_test_util:dummy_user(),
  UserWithKey = User#user{pubkey="apubkey"},
  Result = gitolite_config:write_pubkey(UserWithKey),
  ?assertEqual(ok, Result),
  {ok, Data} = file:read_file( filename:join([?GITOLITE_REPO, "keydir",
                                        User#user.email ++ ".pub"])),
  ?assertEqual("apubkey", binary_to_list(Data)),
  passed.

test_app_config_info() ->
  DummyApp = bh_test_util:dummy_app(),
  User = bh_test_util:dummy_user(),
  %% Make sure app has a user
  user_apps:create(User, DummyApp),
  Info = gitolite_config:app_config_info(DummyApp),

  ?assertEqual(expected_app_info(DummyApp, User), Info),
  passed.

test_write_config_file() ->
  DummyApp = bh_test_util:dummy_app(),
  {ok, App} = apps:create(DummyApp),
  User = bh_test_util:dummy_user(),
  %% Make sure app has a user
  user_apps:create(User, App),

  ?assertEqual(ok, gitolite_config:write_config_file()),

  Filename = filename:join([?GITOLITE_REPO, "conf", "gitolite.conf"]),
  {ok, File} = file:read_file(Filename),
  ?assertEqual(expected_app_info(App, User), binary_to_list(File)),
  passed.


expected_app_info(App, User) ->
 "  repo " ++ App#app.name ++ "\n    RW+ = " ++ User#user.email ++ " \n\n".
