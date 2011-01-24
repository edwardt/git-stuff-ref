%%% beehive_db_srv.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 05/28/10 Ari Lerner <arilerner@mac.com>
%% @doc Database server
%%
%% db_srv expects to be initialized with a database adapter. The only
%% adapter currently implemented is mnesia.
%% Any adapter is expected to implement the following exports.
%%
%%   start/1,
%%   stop/0,
%%   read/2,
%%   all/1,
%%   save/1,
%%   write/3,
%%   run/1,
%%   delete/2,
%%   delete_all/1,
%%   match/1,
%%   info/1
%%
%%  This mimics the API of the actual beehive_db_srv below.  Check out that
%%  code to see what arguments are provided.

-module (beehive_db_srv).

-behaviour(gen_server).

%% API
-export([
  start_link/0, start_link/1, start_link/2,
  stop/0,
  read/2,
  write/3,
  delete/2, delete_all/1,
  all/1,
  run/1,
  save/1,
  status/0,
  match/1,
  info/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% For testing
-export ([init_databases/0]).

-record(state, {
  adapter
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
% Get the status of the db
status() -> gen_server:call(?SERVER, {status}).
read(Table, Key) -> gen_server:call(?SERVER, {read, Table, Key}).
write(Table, Key, Proplist) ->
  gen_server:call(?SERVER, {write, Table, Key, Proplist}).
save(Function) -> gen_server:call(?SERVER, {save, Function}).
delete(Table, Key) -> gen_server:call(?SERVER, {delete, Table, Key}).
delete_all(Table) -> gen_server:call(?SERVER, {delete_all, Table}).
all(Table) -> gen_server:call(?SERVER, {all, Table}).
run(Fun) -> gen_server:call(?SERVER, {run, Fun}).
match(Mod) -> gen_server:call(?SERVER, {match, Mod}).

init_databases() -> gen_server:cast(?SERVER, {init_databases}).
info(Type) -> gen_server:call(?SERVER, {info, Type}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> start_link(mnesia, []).

start_link(DbAdapter) when is_atom(DbAdapter) ->
  start_link(erlang:atom_to_list(DbAdapter));
start_link(DbAdapter) -> start_link(DbAdapter, []).

start_link(DbAdapter, Nodes) when is_atom(DbAdapter) ->
  start_link(erlang:atom_to_list(DbAdapter), Nodes);
start_link(DbAdapter, Nodes) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [DbAdapter, Nodes], []).

stop() -> gen_server:cast(?SERVER, {stop}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([DbAdapterName, Nodes]) ->
  DbAdapter =
    erlang:list_to_atom(lists:flatten(["db_", DbAdapterName, "_adapter"])),
  init_adapter([node()|Nodes], DbAdapter),

  {ok, #state{
    adapter = DbAdapter
  }}.
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({write, Table, Key, Proplist}, From, State) ->
  {reply, call_adapter(write, [Table, Key, Proplist], State), State};
handle_call({save, Fun}, From, State) ->
  {reply, call_adapter(save, [Fun], State), State};
handle_call({read, Table, Key}, From, State) ->
  {reply, call_adapter(read, [Table, Key], State), State};
% Deletions
handle_call({delete, Table, Key}, From, State) ->
  {reply, call_adapter(delete, [Table, Key], State), State};
handle_call({delete_all, Table}, From, State) ->
  {reply, call_adapter(delete_all, [Table], State), State};
handle_call({status}, From, State) ->
  {reply, call_adapter(status, [], State), State};
handle_call({all, Table}, From, State) ->
  {reply, call_adapter(all, [Table], State), State};
handle_call({run, Fun}, From, State) ->
  {reply, call_adapter(run, [Fun], State), State};
handle_call({match, Mod}, From, State) ->
  {reply, call_adapter(match, [Mod], State), State};
handle_call({info, Type}, From, State) ->
  {reply, call_adapter(info, [Type], State), State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({init_databases}, #state{adapter = Adapter} = State) ->
  init_adapter([node(self())], Adapter),
  {noreply, State};
handle_cast({stop}, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(Info, State) ->
  erlang:display({?MODULE, handle_info, Info}),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

call_adapter(F, A, State) ->
  apply(State#state.adapter,F,A).

init_adapter(Nodes, DbAdapter) ->
  case erlang:module_loaded(DbAdapter) of
    true -> ok;
    false ->
      case code:load_file(DbAdapter) of
        {error, not_purged} -> code:purge(DbAdapter), code:load_file(DbAdapter);
        {error, _Error} = T -> throw(T);
        _ -> ok
      end
  end,
  case erlang:function_exported(DbAdapter, init_databases, 1) of
    true -> apply(DbAdapter, init_databases, [Nodes]);
    false -> ok
  end,
  ok.
