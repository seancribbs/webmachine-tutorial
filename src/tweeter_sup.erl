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
                {erlang:now(), [{avatar, <<"https://si0.twimg.com/profile_images/2536088319/4sl2go65was3o0km520j_reasonably_small.jpeg">>}, {message, <<"Caremad.">>}]},
                {erlang:now(), [{avatar, <<"https://si0.twimg.com/profile_images/3778090444/e4fde2cad4b921cd8c07fcecc0ff2fff_bigger.jpeg">>}, {message, <<"Rubby is over!">>}]},
                {erlang:now(), [{avatar, <<"https://si0.twimg.com/profile_images/2536088319/4sl2go65was3o0km520j_reasonably_small.jpeg">>}, {message, <<"You boys having a taste?">>}]}
            ]),

    %% Register a pg2 group.
    ok = pg2:create(tweets),

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
