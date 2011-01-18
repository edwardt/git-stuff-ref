-module (users_controller_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").

setup() ->
  bh_test_util:setup(),
  bh_test_util:dummy_user(),                    % test@getbeehive.com
  rest_server:start_link(),
  timer:sleep(100),
  ok.

teardown(_X) ->
  beehive_db_srv:delete_all(user),
  beehive_db_srv:delete_all(user_app),
  beehive_db_srv:delete_all(app),
  ok.

starting_test_() ->
  {inorder,
   {setup,
    fun setup/0,
    fun teardown/1,
    [
     fun get_index/0,
     fun get_index_with_email/0,
     fun get_index_with_bad_email/0,
     fun get_user_apps_no_apps/0,
     fun get_user_apps_with_bad_email/0,
     fun get_user_apps_with_an_app/0,
     fun post_new_user/0,
     fun post_new_user_bad_auth/0,
     fun post_new_user_non_admin_auth/0,
     fun post_user_pubkeys_as_admin/0,
     fun post_user_pubkeys_as_user/0,
     fun post_user_pubkeys_as_wrong_user/0,
     fun post_user_pubkeys_no_pubkey_provided/0,
     fun post_new_user_adds_pubkey/0
    ]
   }
  }.

get_index() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [Json|_] = bh_test_util:response_json(Response),
  {"users", Users} = Json,
  ?assert(is_list(Users)),
  ?assert(lists:any(fun(E) ->
                        proplists:get_value("email", E) =:= "test@getbeehive.com"
                    end, Users)),
  passed.

get_index_with_email() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/test@getbeehive.com.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [User|_] = bh_test_util:response_json(Response),
  {"user",[{"email","test@getbeehive.com"}|_]} = User,
  passed.

get_index_with_bad_email() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/noexist@nope.com.json"}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  passed.

get_user_apps_no_apps() ->
  lists:foreach(fun(UApp) -> user_apps:delete(UApp) end,
                user_apps:find_all_by_email("test@getbeehive.com")),

  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/test@getbeehive.com/apps.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [User|_] = bh_test_util:response_json(Response),
  {"apps",""} = User,
  passed.

get_user_apps_with_bad_email() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/noexist@nope.com/apps.json"}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  passed.

get_user_apps_with_an_app() ->
  DummyUser = bh_test_util:dummy_user(),
  App = bh_test_util:dummy_app(),
  {ok, SavedApp} = apps:save(App),
  {ok, _} = user_apps:create(DummyUser, SavedApp),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, "/users/test@getbeehive.com/apps.json"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [Apps|_] = bh_test_util:response_json(Response),
  %% Two arrays: 1 for the list, 1 for the object proplist
  ?assertMatch({"apps", [[{"name",_}]]}, Apps),
  passed.

post_new_user() ->
  Admin = bh_test_util:admin_user(),
  {ok, Header, Response} =
    perform_post_new( [
                     {email, "createduser@bhive.com"},
                     {password, "created"},
                     {token, Admin#user.token }
                   ]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [{"user", User}|_] = bh_test_util:response_json(Response),
  ?assertMatch([{"email", "createduser@bhive.com"}], User),
  passed.

post_new_user_bad_auth() ->
  {ok, Header, Response} =
    perform_post_new( [
                     {email, "createduser@bhive.com"},
                     {password, "created"},
                     {token, "unauthed" }
                   ]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  ?assertMatch("Unauthorized.",
               bh_test_util:response_json(Response)),
  passed.

post_new_user_non_admin_auth() ->
  RegUser = bh_test_util:dummy_user(),
  {ok, Header, Response} =
    perform_post_new( [
                     {email, "createduser@bhive.com"},
                     {password, "created"},
                     {token, RegUser#user.token }
                   ]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  ?assertMatch("Unauthorized.",
               bh_test_util:response_json(Response)),
  passed.

post_user_pubkeys_as_admin() ->
  erlymock:start(),
  User = bh_test_util:dummy_user(),
  Admin = bh_test_util:admin_user(),
  erlymock:stub(beehive_repository, add_user_pubkey,
                [User#user.email, "newkey"]),
  erlymock:replay(),
  {ok, Header, Response} =
    perform_post_pubkeys(User#user.email,
                         [{token, Admin#user.token},
                          {pubkey, "newkey"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  UpdatedUser = users:find_by_email(User#user.email),
  ?assertEqual("newkey", UpdatedUser#user.pubkey),
  erlymock:verify(),
  passed.

post_user_pubkeys_as_user() ->
  erlymock:start(),
  User = bh_test_util:dummy_user(),
  Key = "newkey",
  erlymock:stub(beehive_repository, add_user_pubkey,
                [User#user.email, Key]),
  erlymock:replay(),
  {ok, Header, Response} =
    perform_post_pubkeys(User#user.email,
                         [{token, User#user.token},
                          {pubkey, Key}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  UpdatedUser = users:find_by_email(User#user.email),
  ?assertEqual(Key, UpdatedUser#user.pubkey),
  erlymock:verify(),
  passed.

post_user_pubkeys_as_wrong_user() ->
  User = bh_test_util:dummy_user(),
  CallingUser =
    bh_test_util:create_user(
      #user{email    = "caller@getbeehive.com",
            password = "test",
            token    = "callertoken"}),
  {ok, Header, Response} =
    perform_post_pubkeys(User#user.email,
                         [{token, CallingUser#user.token},
                          {pubkey, "newkey"}]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  passed.

post_new_user_adds_pubkey() ->
  Key = "pubkey",

  erlymock:start(),
  erlymock:stub(beehive_repository, add_user_pubkey,
                ["pkuser@bhive.com", Key]),
  erlymock:replay(),
  Admin = bh_test_util:admin_user(),
  {ok, Header, Response} =
    perform_post_new( [
                     {email, "pkuser@bhive.com"},
                     {password, "created"},
                     {token, Admin#user.token },
                     {pubkey, Key}
                   ]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  erlymock:verify(),
  passed.

post_user_pubkeys_no_pubkey_provided() ->
  User = bh_test_util:dummy_user(),
  {ok, Header, Response} =
    perform_post_pubkeys(User#user.email,
                         [{token, User#user.token}]),
  ?assertEqual("HTTP/1.0 400 Bad Request", Header),
  passed.

perform_post_new(Params) ->
  bh_test_util:fetch_url(post,
                         [{path, "/users.json"},
                          {headers, [{"Content-Type",
                                      "application/x-www-form-urlencoded" }]},
                          {params, Params}
                         ]).

perform_post_pubkeys(Email, Params) ->
  bh_test_util:fetch_url(post,
                         [{path, lists:flatten(["/users/", Email,
                                                "/pubkeys.json"])},
                          {headers, [{"Content-Type",
                                      "application/x-www-form-urlencoded" }]},
                          {params, Params}
                         ]).

