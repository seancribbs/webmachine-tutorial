-module(tweeter_app).

-behaviour(application).

-export([start/2,
         stop/1]).

%% @doc application start callback for tweeter.
start(_Type, _StartArgs) ->
    tweeter_sup:start_link().

%% @doc application stop callback for tweeter.
stop(_State) ->
    ok.
