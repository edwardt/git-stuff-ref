%%% @author Jacob Dunphy
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2010 by Jacob Dunphy


-module(beehive_repository).
-behaviour(gen_server).

%% API
-export([start_link/0,
         create/1,
         add_user_to_repository/2,
         remove_user_from_repository/2,
         clone/2,
         clone_url/1,
         add_user_pubkey/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { name, pid }).
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @doc  Set up a repository in the git backend
%% @spec create(Name) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
create(Name) ->
  gen_server:call(?SERVER, {create, Name}).

%%--------------------------------------------------------------------
%% @doc
%% If git backend has permission requirements (like gitolite), then
%% add configuration to provide user with RW permissions.
%%
%% @spec add_user_to_repository(Username, Name) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
add_user_to_repository(Username, Name) ->
  gen_server:call(?SERVER, {add_user, Username, Name}).

%%--------------------------------------------------------------------
%% @doc
%% If git backend has permission requirements (like gitolite), then
%% updated config to remove RW permissions for user.
%%
%% @spec remove_user_from_repository(Username, Name) ->
%          ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
remove_user_from_repository(Username, Name) ->
  gen_server:call(?SERVER, {remove_user, Username, Name}).

%%--------------------------------------------------------------------
%% @doc  Performs a git clone of a repository, using Path as its
%%       destination in the file system.
%% @spec clone(Name, Path) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
clone(Name, Path) ->
  gen_server:call(?SERVER, {clone, Name, Path}).

%%--------------------------------------------------------------------
%% @doc  Provides either a ssh or local filesystem path that can be used
%%       as a git remote for pushing to the git backend.
%% @spec clone_url(Name) -> {ok, Url} | {error, Error}
%% @end
%%--------------------------------------------------------------------
clone_url(Name) ->
  gen_server:call(?SERVER, {clone_url, Name}).

%%--------------------------------------------------------------------
%% @doc  Save a public ssh key for a given user Name.
%% @spec add_user_pubkey(Name, Key) -> ok | {error, Error}
%%          Name = string()
%%          Key  = binary()
%% @end
%%--------------------------------------------------------------------
add_user_pubkey(Name, Key) ->
  gen_server:call(?SERVER, {add_pubkey, Name, Key}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
  Adapter = config:search_for_application_value(repository, gitolite),
  {ok,Pid} = case list_to_atom(Adapter) of
    local_git ->
      gen_server:start(local_git, [], []);
    gitolite ->
      gen_server:start(glitter_adapter, [], [])
  end,
  {ok, #state{ pid = Pid, name = Adapter }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
  Response = gen_server:call(State#state.pid, Request),
  {reply, Response, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
