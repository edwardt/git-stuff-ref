%%%-------------------------------------------------------------------
%%% @author Jacob Dunphy
%%% @doc
%%%
%%% @end
%%% Created : 13 Dec 2010 by Jacob Dunphy
%%%-------------------------------------------------------------------
-module(glitter_adapter).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, sanitize_name/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-define(GITOLITE_CONFIG, ?BEEHIVE_HOME ++ "/gitolite/conf/gitolite.conf").
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
init([]) ->
  gen_server:start({global, glitter}, glitter,
                   [{config_file, ?GITOLITE_CONFIG}], []),
  {ok, #state{}}.

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
  Resp = glitter:add_repos(Name),
  glitter:commit(),
  {reply, Resp, State};
handle_call({add_user, Username, Name}, _From, State) ->
  glitter:add_user_to_repos({Username, "RW+"}, Name),
  glitter:commit(),
  {reply, ok, State};
handle_call({remove_user, Username, Name}, _From, State) ->
  glitter:remove_user_from_repos(Username, Name),
  glitter:commit(),
  {reply, ok, State};
handle_call({clone, _Name, _Path}, _From, State) ->
  {reply, ok, State};
handle_call({clone_url, Name}, _From, State) ->
  Domain = config:search_for_application_value(domain),
  Url = lists:flatten(["beehive@",Domain ,":", Name, ".git"]),
  {reply, Url ,State};
handle_call({add_pubkey, Name, Key}, _From, State) ->
  ok = glitter:add_user((key_filename(Name), Key),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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

%% The Name we're receiving is usually an email. gitolite doesn't mind
%% periods, hyphens and underscores, but it won't handle @'s.
sanitize_name([]) -> [];
sanitize_name([$@|Rest]) -> "-" ++ sanitize_name(Rest);
sanitize_name([Ok|Rest]) -> [Ok] ++ sanitize_name(Rest).
