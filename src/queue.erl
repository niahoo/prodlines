%%%-------------------------------------------------------------------
%%% File    : queue.erl
%%% Author  : Ludovic <ludovic@meelux>
%%% Description : 
%%%
%%% Created :  2 May 2011 by Ludovic <ludovic@meelux>
%%%-------------------------------------------------------------------
-module(queue).

-behaviour(gen_server).

-include("task.hrl").

%% API
-export([start_link/1, enqueue/2, get_list/1,finished_signal/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {idle, name, props, tasks, taskLastId=0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link({Name, Props}) ->
    gen_server:start_link(?MODULE, [{Name, Props}], []).

enqueue(PID, Task) ->
    case task_utils:validate(Task) of
	ok -> gen_server:call(PID, {enqueue, Task});
	error -> error
    end.

get_list(_) ->
    ok.
finished_signal(PID, ID) ->
    gen_server:cast(PID, {finished, ID}).
    

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
init([{Name, Props}]) ->
    io:format("New queue started, name is ~p~n"
	      "Props are ~p~n~n", [Name,Props]),
    {ok, #state{idle=true,name=Name,props=Props,tasks=[]}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({enqueue, Task}, _From, State) ->
    NewId = State#state.taskLastId + 1,
    NewQueue = lists:append(State#state.tasks, [{NewId, Task}]),
    NewState = State#state{tasks=NewQueue,taskLastId=NewId},
    Reply = {reply, ok, NewState},
    check_for_launch(Reply)
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
handle_cast({finished, _ID}, #state{tasks=Tasks}=State) when Tasks == [] ->
    {noreply, State};
handle_cast({finished, ID}, State) ->
    Tasks = State#state.tasks,
    [{CheckID, Task}|Tail] = Tasks,
    Reply = 
	case CheckID == ID of 
	    true -> spawn(fun() -> notify_finished(Task, State) end),
		    {noreply, State#state{idle=true, tasks=Tail}} ;
	    false -> 
		io:format("Unexpected finished task ~p~n", [ID]),
		%% notify_finished(Task, State) %% Comportement anormal
		{noreply, State#state{idle=true}}
	end,
    check_for_launch(Reply)
	;


    

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
check_for_launch({reply, Reply, State}) ->
    {reply, Reply, launch(State)};
	    
check_for_launch({noreply, State}) ->
    {noreply, launch(State)}.

launch(State) ->
    case State#state.idle of 
	false -> State ;
	true -> 
	    case State#state.tasks of
		[{ID,Task}|_T] ->
		    timer:apply_after(Task#task.duration, ?MODULE, finished_signal, [self(),ID]),
		    State#state{idle=false} ;
		[] ->
		    io:format("No more tasks in queue~n"),
		    State
	    end
    end.

%% @todo définir la fonction. si la task à une callback_fun, la lancer, 
%% sinon si elle a une callback url définie, l'appeler avec un set
%% d'arguments prédéfinis
%% sinon si la state.props.default_callback_url est définie, l'appeler
%% avec ce même set
notify_finished(Task, _State) ->
    io:format("Task is finished ~p~n", [Task]).
