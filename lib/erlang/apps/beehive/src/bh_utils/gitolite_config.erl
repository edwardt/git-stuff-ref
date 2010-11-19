%%% @author Jacob Dunphy <jdunphy@YPCMC09282>
%%% @doc
%%%
%%% @end
%%% Created : 17 Nov 2010

-module(gitolite_config).

-include("beehive.hrl").
-include("common.hrl").

-export([write_pubkey/1,
         write_config_file/0,
         app_config_info/1]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

write_pubkey(User) when is_record(User, user) ->
  Key = User#user.pubkey,
  Filename = filename:join([?GITOLITE_REPO, "keydir",
                            user_key_name(User) ++ ".pub"]),
  filelib:ensure_dir(Filename),
  file:write_file(Filename, list_to_binary(Key)).

write_config_file() ->
  Apps = apps:all(),
  Data = lists:flatten(lists:map(fun(A) -> app_config_info(A) end, Apps)),

  Filename = filename:join([?GITOLITE_REPO, "conf", "gitolite.conf"]),
  filelib:ensure_dir(Filename),

  file:write_file(Filename, list_to_binary(Data)).

app_config_info(App) when is_record(App, app) ->
  Users = user_apps:get_users(App),
  Names = lists:map(fun(U) -> user_key_name(U) ++ " " end, Users),
  RepoLine = "  repo " ++ App#app.name ++ "\n",
  lists:flatten([RepoLine, "    RW+ = ", Names, "\n\n"]).

user_key_name(User) ->
  User#user.email.
