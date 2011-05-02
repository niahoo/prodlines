%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the prodlines application.

-module(prodlines_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for prodlines.
start(_Type, _StartArgs) ->
    prodlines_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for prodlines.
stop(_State) ->
    ok.
