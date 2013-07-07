-module(tweeter_wm_tweet_resource).

-export([init/1,
         routes/0,
         to_json/2,
         forbidden/2,
         generate_etag/2,
         resource_exists/2,
         content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {tweet}).

%% @doc Initialize the resource.
-spec init([]) -> {ok, #context{}}.
init([]) ->
    {ok, #context{}}.

%% @doc Return the routes this module should respond to.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{["tweets", tweet_id], ?MODULE, []}].

%% @doc Validate CSRF token.
-spec forbidden(wrq:reqdata(), #context{}) ->
    {boolean(), wrq:reqdata(), #context{}}.
forbidden(ReqData, Context) ->
    {tweeter_security:is_protected(ReqData, Context), ReqData, Context}.

%% @doc Extract the identifier out of the URL.
-spec tweet_id(wrq:reqdata()) -> {integer(), integer(), integer()}.
tweet_id(ReqData) ->
    time_from_timestamp(wrq:path_info(tweet_id, ReqData)).

%% @doc Provide only application/json content.
-spec content_types_provided(wrq:reqdata(), #context{}) ->
    {list({list(), atom()}), wrq:reqdata(), #context{}}.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% @doc Generate etag.
-spec generate_etag(wrq:reqdata(), #context{}) ->
    {binary(), wrq:reqdata(), #context{}}.
generate_etag(ReqData, Context) ->
    {_, NewContext} =  maybe_retrieve_tweet(Context, tweet_id(ReqData)),
    ETag = mochihex:to_hex(erlang:phash2(NewContext#context.tweet)),
    {ETag, ReqData, NewContext}.

%% @doc Determine if the tweet exists.
-spec resource_exists(wrq:reqdata(), #context{}) ->
    {boolean(), wrq:reqdata(), #context{}}.
resource_exists(ReqData, Context) ->
    case maybe_retrieve_tweet(Context, tweet_id(ReqData)) of
        {true, NewContext} ->
            {true, ReqData, NewContext};
        {false, Context} ->
            {false, ReqData, Context}
    end.

%% @doc Serialize the tweet.
-spec to_json(wrq:reqdata(), #context{}) ->
    {binary(), wrq:reqdata(), #context{}}.
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
-spec maybe_retrieve_tweet(#context{}, term()) -> {boolean(), #context{}}.
maybe_retrieve_tweet(Context, TweetId) ->
    case Context#context.tweet of
        undefined ->
            case ets:lookup(tweets, TweetId) of
                [] ->
                    {false, Context};
                [{_Key, Tweet}] ->
                    {true, Context#context{tweet=Tweet}}
            end;
        _ ->
            {true, Context}
    end.

%% @doc Convert time from unix time.
-spec time_from_timestamp(list()) -> {integer(), integer(), integer()}.
time_from_timestamp(Timestamp0) ->
    Timestamp = list_to_integer(Timestamp0),
    {Timestamp div 1000000000000,
     Timestamp div 1000000 rem 1000000,
     Timestamp rem 1000000}.
