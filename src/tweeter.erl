-module(tweeter).
-export([start/0, start_link/0, stop/0]).

%% @doc Ensure application is started.
-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @doc Starts the app for inclusion in a supervisor tree
-spec start_link() -> {ok,Pid::pid()}.
start_link() ->
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(lager),
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_log),
    ensure_started(webmachine),
    tweeter_sup:start_link().

%% @doc Start the tweeter server.
-spec start() -> ok.
start() ->
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(lager),
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_log),
    ensure_started(webmachine),
    application:start(tweeter).

%% @doc Stop the tweeter server.
-spec stop() -> ok.
stop() ->
    Res = application:stop(tweeter),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    application:stop(lager),
    application:stop(syntax_tools),
    application:stop(compiler),
    Res.
