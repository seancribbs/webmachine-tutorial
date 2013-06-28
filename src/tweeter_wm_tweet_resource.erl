-module(tweeter_wm_tweet_resource).

-export([init/1,
         routes/0,
         to_json/2,
         resource_exists/2,
         content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {tweet}).

%% @doc Initialize the resource.
init([]) ->
    {ok, #context{}}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{["tweets", tweet_id], ?MODULE, []}].

%% @doc Extract the identifier out of the URL.
tweet_id(ReqData) ->
    time_from_timestamp(wrq:path_info(tweet_id, ReqData)).

%% @doc Provide only application/json content.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% @doc Determine if the tweet exists.
resource_exists(ReqData, Context) ->
    case maybe_retrieve_tweet(Context, tweet_id(ReqData)) of
        {true, NewContext} ->
            {true, ReqData, NewContext};
        {false, Context} ->
            {false, ReqData, Context}
    end.

%% @doc Serialize the tweet.
to_json(ReqData, Context) ->
    case maybe_retrieve_tweet(Context, tweet_id(ReqData)) of
        {true, NewContext} ->
            Tweet = NewContext#context.tweet,
            Response = mochijson2:encode({struct, [{tweet, Tweet}]}),
            {Response, ReqData, NewContext};
        {false, Context} ->
            Response = mochijson2:encode({struct, []}),
            {Response, ReqData, Context}
    end.

%% @doc Either get the tweet and store in context, or attempt to lookup.
maybe_retrieve_tweet(Context, TweetId) ->
    case Context#context.tweet of
        undefined ->
            case ets:lookup(tweets, TweetId) of
                [] ->
                    io:format("no results!!!"),
                    {false, Context};
                [{_Key, Tweet}] ->
                    {true, Context#context{tweet=Tweet}}
            end;
        _ ->
            {true, Context}
    end.

%% @doc Convert time from unix time.
time_from_timestamp(Timestamp0) ->
    Timestamp = list_to_integer(Timestamp0),
    {Timestamp div 1000000000000,
     Timestamp div 1000000 rem 1000000,
     Timestamp rem 1000000}.
