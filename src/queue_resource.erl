%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(queue_resource).
-export([init/1,
	 allowed_methods/2,
	 content_types_accepted/2,
	 content_types_provided/2,
	 create_queue/2
	]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {{trace, "/tmp"}, undefined}.

allowed_methods(Req, State) ->
    {['PUT','GET'], Req, State}.

content_types_accepted(Req, State) ->
   {[{"application/json", create_queue}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", queue_info}], Req, State}.

create_queue(Req, State) ->
    NewQueueName = wrq:path_info(name, Req),
    Reply = case prodlines_server:create_queue(NewQueueName) of
		ok -> true ;
		exists -> {halt, 409}
	    end,
    io:format("~p~n", [Reply]),
    {Reply, Req, State}.
		    
	    
