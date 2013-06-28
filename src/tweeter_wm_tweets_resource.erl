-module(tweeter_wm_tweets_resource).

-export([init/1,
         routes/0,
         to_json/2,
         content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

%% @doc Initialize the resource.
init([]) ->
    {ok, #context{}}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{["tweets"], ?MODULE, []}].

%% @doc Provide only application/json content.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% @doc Return the list of tweets.
to_json(ReqData, Context) ->
    Tweets = [Value ++ [{id, time_to_timestamp(Key)}] ||
                [{Key, Value}] <- ets:match(tweets, '$1')],
    Content = mochijson2:encode({struct, [{tweets, Tweets}]}),
    {Content, ReqData, Context}.

%% @doc Convert time to unix time.
time_to_timestamp({Mega, Sec, Micro}) ->
    Time = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro,
    list_to_binary(integer_to_list(Time)).
