%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(queue_resource).
-export([init/1,
	 allowed_methods/2,
	 resource_exists/2,
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

resource_exists(Req, State) ->
    Exists = prodlines_server:queue_exists(wrq:path_info(name, Req)),
    io:format("Resource Exists: ~p~n", [Exists]),
    {Exists, Req, State}.

create_queue(Req, State) ->
    NewQueueName = wrq:path_info(name, Req),
    Props =  try mochijson:decode(wrq:req_body(Req)) of
		 {struct, Options} -> Options 
	     catch
		 error:_ -> []
	     end,
    io:format("Queue options: ~p~n", [Props]),
    {Reply, Req2} = 
	case prodlines_server:create_queue(NewQueueName, Props) of
	    ok -> 
		%% ajout du header location pour que la réponse soit 201
		{true, wrq:set_resp_header("Location", wrq:path(Req), Req)};
	    exists -> 
		{{halt, 409}, Req}
	    end,
    io:format("Queue Created: ~p~n", [Reply]),
    {Reply, Req2, State}.
		    
	    

