-module(task_utils).
-export([validate/1]).

-include("task.hrl").

validate(#task{duration=Duration,identifier=ID}) ->
    case is_integer(Duration) and is_list(ID) of
	true -> ok ;
	false -> error
    end;
validate(_) -> error.

	
