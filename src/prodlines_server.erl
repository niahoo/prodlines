%%%-------------------------------------------------------------------
%%% File    : prodlines_server.erl
%%% Author  : Ludovic <ludovic@meelux>
%%% Description : 
%%%
%%% Created :  2 May 2011 by Ludovic <ludovic@meelux>
%%%-------------------------------------------------------------------
-module(prodlines_server).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
%% API
-export([start_link/0,create_queue/2,delete_queue/1,queue_exists/1,queue_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {queues}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
create_queue(Name, Props) ->
    gen_server:call(?SERVER, {create, Name, Props}).
delete_queue(Name) ->
    gen_server:call(?SERVER, {delete, Name}).
queue_exists(Name) ->
    gen_server:call(?SERVER, {exists, Name}).
queue_pid(Name) ->
    gen_server:call(?SERVER, {getpid, Name}).
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
init([]) ->
    Queues = ets:new(queues, [set,private]),
    {ok, #state{queues=Queues}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({create, Name, Props}, _From, State) ->
    Exists = case ets:lookup(State#state.queues, Name) of
		[] -> false ;
		[_] -> true
	    end,
    case Exists of
	true -> {reply, exists, State} ;
	false ->
	    {ok, NewQueue} = queue:start_link({Name, Props}),
	    Reply = 
		case ets:insert_new(State#state.queues, {Name, NewQueue}) of
		    true -> ok ;
		    false -> exists
		end,
	    {reply, Reply, State}
    end
	;

handle_call({delete, Name}, _From, State) ->
    ok
	;
handle_call({exists, Name}, _From, State) ->
    Reply = case ets:lookup(State#state.queues, Name) of
		[] -> false ;
		[_] -> true
	    end,
    {reply, Reply, State}
	;
handle_call({getpid, Name}, _From, State) ->
    Reply = case ets:lookup(State#state.queues, Name) of
		[{Name, PID}] -> PID;
		_ -> doesnt_exists
	    end,
    {reply, Reply, State}
	;
		
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
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
 
