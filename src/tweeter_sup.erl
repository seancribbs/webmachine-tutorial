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

    Resources = [tweeter_wm_tweets_resource,
                 tweeter_wm_tweet_resource,
                 tweeter_wm_asset_resource],

    Dispatch = lists:flatten([Module:routes() || Module <- Resources]),

    %% Create an ETS table for storing the tweets.
    _ = ets:new(tweets, [public,
                         ordered_set,
                         named_table,
                         {read_concurrency, true},
                         {write_concurrency, true}]),

    %% Seed the database with some tweets.
    _ = ets:insert(tweets, [
                {erlang:now(), [{avatar, <<"http://upload.wikimedia.org/wikipedia/en/thumb/f/f4/The_Wire_Jimmy_McNulty.jpg/250px-The_Wire_Jimmy_McNulty.jpg">>}, {message, <<"Pawns.">>}]},
                {erlang:now(), [{avatar, <<"http://upload.wikimedia.org/wikipedia/en/thumb/1/15/The_Wire_Bunk.jpg/250px-The_Wire_Bunk.jpg">>}, {message, <<"A man's gotta have a code.">>}]},
                {erlang:now(), [{avatar, <<"http://upload.wikimedia.org/wikipedia/en/thumb/f/f4/The_Wire_Jimmy_McNulty.jpg/250px-The_Wire_Jimmy_McNulty.jpg">>}, {message, <<"You boys have a taste?">>}]}
            ]),

    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],

    %% This is the web-server listener and dispatcher.
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},

    %% This is the publish-subscribe process, a gen_event.
    Events = {tweeter_events,
              {tweeter_events, start_link, []},
              permanent, 5000, worker, [tweeter_events]},

    Processes = [Events, Web],

    {ok, { {one_for_one, 10, 10}, Processes} }.
