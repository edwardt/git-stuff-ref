%%%-------------------------------------------------------------------
%%% File    : users_controller.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Sat Nov 28 23:03:51 PST 2009
%%%-------------------------------------------------------------------

-module (users_controller).

-include ("beehive.hrl").
-include ("http.hrl").
-export ([get/2, post/2, put/2, delete/2]).

get([Email, "apps"], _Data) ->
  case users:find_by_email(Email) of
    not_found -> {error, 404, {Email, "does_not_exist"}};
    User ->
      Apps = user_apps:all_apps(User#user.email),
      {apps, lists:map(fun(App) -> [{name, App#app.name}] end, Apps)}
  end;
get([Email], _Data) ->
  case users:find_by_email(Email) of
    not_found -> {error, 404, {Email, "does_not_exist"}};
    User -> {
      "user", [{"email", User#user.email},
               {"level", User#user.level},
               {"pubkey", User#user.pubkey}]
     }
  end;
get(_, _Data) ->
  All = users:all(),
  { "users", lists:map(fun(A) ->
                           [{"level", A#user.level}, {"email", A#user.email}]
                       end, All)
  }.

post([Name, "pubkeys"], Data) ->
  case proplists:get_value(pubkey, Data) of
    undefined -> app_error("No pubkey defined", 400);
    Pubkey ->
      case auth_utils:get_authorized_user(Data) of
        AuthorizedUser when is_record(AuthorizedUser, user) ->
          case auth_utils:is_admin_user(AuthorizedUser) of
            true ->
              case users:find_by_email(Name) of
                User when is_record(User, user) ->
                  add_pubkey(User, Pubkey);
                _E ->
                  app_error("Error finding user")
              end;
            false ->
              Email = AuthorizedUser#user.email,
              case Name of
                Email ->
                  add_pubkey(AuthorizedUser, Pubkey);
                _Err ->
                  app_error("You can not add a key for another user", 401)
              end
          end
      end
  end;

post([], Data) ->
  auth_utils:run_if_admin(
    fun(_) ->
        case proplists:get_value(email, Data) of
            undefined -> {error, 400, "The email field was not provided."};
            Email ->
                %% The user has been submitted with an email
                case users:exist(Email) of
                    true ->
                        {error, 409,
                            "A user with that email address already exists."};
                    false ->
                        case users:create(Data) of
                            {ok, User} when is_record(User, user) ->
                                {user, [{email, User#user.email}]};
                            E ->
                                io:format("Error: ~p~n", [E]),
                                {error, 500, "There was an error creating user"}
                        end
                end
        end
end, Data);

post(Path, _Data) ->
  io:format("Path: ~p~n", [Path]),
  app_error("unhandled").
put(_Path, _Data) -> "unhandled".

delete([], Data) ->
  auth_utils:run_if_admin(fun(_) ->
                              case proplists:is_defined(email, Data) of
                                false -> misc_utils:to_bin("No email given");
                                true ->
                                  Email = proplists:get_value(email, Data),
                                  users:delete(Email)
                              end
                          end, Data);
delete(_Path, _Data) -> "unhandled".

add_pubkey(User, Pubkey) ->
  case users:save(User#user{pubkey = Pubkey}) of
    {ok, SavedUser} ->
      ok = beehive_repository:add_user_pubkey(SavedUser#user.email, Pubkey),
      [{"user", SavedUser#user.email}, {"pubkey", "added pubkey"}];
    _Else ->
      app_error("There was an error updating the user")
  end.

app_error(Msg, Status) ->
  {error, Status, misc_utils:to_bin(Msg)}.

app_error(Msg) ->
  {error, misc_utils:to_bin(Msg)}.
