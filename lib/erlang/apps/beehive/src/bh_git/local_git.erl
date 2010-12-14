%%%-------------------------------------------------------------------
%%% @author Jacob Dunphy
%%% @doc
%%% local_git should be used as a git backend for local dev and testing.
%%% There is no handling of user or repo permissions, those API endpoints
%%% will simply pass the expected success response for now.  The only
%%% calls with actual logic are create, clone, and clone_url.
%%%
%%% @end
%%% Created :  9 Dec 2010 by Jacob Dunphy
%%%-------------------------------------------------------------------
-module(local_git).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("common.hrl").
-define(LOCAL_REPO_ROOT, ?BEEHIVE_HOME ++ "/git_repos").


-record(state, {repo_dir}).

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
init(_Args) ->
  filelib:ensure_dir(?LOCAL_REPO_ROOT ++ "/.git"),
  {ok, #state{repo_dir = ?LOCAL_REPO_ROOT}}.

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

handle_call({create, Name}, _From, State) ->
  RepoName = Name ++ ".git",
  Status = run_git_command(["init", "--bare", RepoName], State#state.repo_dir),
  erlang:display(Status),
  {reply, ok, State};
handle_call({add_user, Username, Name}, _From, State) ->
  {reply, ok, State};
handle_call({remove_user, Username, Name}, _From, State) ->
  {reply, ok, State};
handle_call({clone, Name, Path}, _From, State) ->
  {reply, ok, State};
handle_call({clone_url, Name}, _From, State) ->
  Url = filename:join(State#state.repo_dir,  Name ++ ".git"),
  {reply, Url ,State};
handle_call({add_pubkey, Name, Key}, _From, State) ->
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

run_git_command(Args, Cd) ->
  Opts = [binary, stderr_to_stdout, use_stdio, exit_status, stream,
          {args, Args}, {cd, Cd}],
  P = open_port({spawn_executable, os:find_executable('git')}, Opts),
  cmd_receive(P).


cmd_receive(Port) ->
   cmd_receive(Port, []).

cmd_receive(Port, Out) ->
  receive
    {Port, {data, Data}}      ->
      List = binary_to_list(Data),
      cmd_receive(Port, [List|Out]);
    {Port, {exit_status, 0}}  ->
      {ok, lists:reverse(Out)};
    {Port, {exit_status, N}}  ->
      {error, {N, lists:reverse(Out)}};
    E ->
      cmd_receive(Port, Out)
    after 500 ->
      % We don't want it to hang infinitely, so if it does, we'll close it off
      ok
  end.

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
