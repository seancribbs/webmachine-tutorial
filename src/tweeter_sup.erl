-module(tweeter_sup).

-behaviour(supervisor).

-export([start_link/0,
         upgrade/0]).

-export([init/1]).

%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),

    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),

    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of
        false ->
            "0.0.0.0";
        Any ->
            Any
    end,

    Port = case os:getenv("PORT") of
        false ->
            8080;
        RawPort ->
            list_to_integer(RawPort)
    end,

    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                         "..", "priv", "dispatch.conf"])),

    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},

    Processes = [Web],

    {ok, { {one_for_one, 10, 10}, Processes} }.
