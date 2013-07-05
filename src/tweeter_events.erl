%% @doc Event manager that broadcasts new tweets to streams.
-module(tweeter_events).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0, notify/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          client :: pid()
         }).


%% @doc Creates an event manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% @doc Adds an event handler. Client processes that want to receive
%% the firehose call this.
-spec add_handler() -> ok | {'EXIT', term()} | term().
add_handler() ->
    %% We use add_sup_handler instead of add_handler so that both the
    %% gen_event and the client process will receive notifications if
    %% either goes down.
    gen_event:add_sup_handler(?SERVER, ?MODULE, [self()]).

%% @doc Sends a notification to all handlers
-spec notify(term()) -> ok.
notify(Msg) ->
    gen_event:notify(?SERVER, Msg).

%% @private
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
-spec init([pid()]) -> {ok, #state{}}.
init([Client]) ->
    {ok, #state{client=Client}}.

%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
-spec handle_event(term(), #state{}) -> {ok, #state{}}.
handle_event({tweet, _}=Msg, #state{client=Client}=State) ->
    Client ! Msg,
    {ok, State};
handle_event(_Ignored, State) ->
    {ok, State}.


%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), #state{}) -> {ok, term(), #state{}}.
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @end
-spec handle_info(term(), #state{}) -> {ok, #state{}}.
handle_info(_Info, State) ->
    {ok, State}.

%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @end
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
