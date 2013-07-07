-module(tweeter_wm_asset_resource).

-export([init/1,
         routes/0,
         to_resource/2,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {filename}).

%% @doc Initialize the resource.
-spec init([]) -> {ok, #context{}}.
init([]) ->
    {ok, #context{}}.

%% @doc Return the routes this module should respond to.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{[""], ?MODULE, []}, {['*'], ?MODULE, []}].

%% @doc Handle serving of the single page application.
-spec allowed_methods(wrq:reqdata(), #context{}) ->
    {list(), wrq:reqdata(), #context{}}.
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

%% @doc Given a series of request tokens, normalize to priv dir file.
-spec normalize_filepath(list()) -> list().
normalize_filepath(Filepath) ->
    {ok, App} = application:get_application(?MODULE),
    filename:join([priv_dir(App), "www"] ++ Filepath).

%% @doc Return a context which determines if we serve up the index or a
%%      particular file
-spec identify_resource(wrq:reqdata(), #context{}) ->
    {boolean(), #context{}}.
identify_resource(ReqData, #context{filename=undefined}=Context) ->
    case wrq:disp_path(ReqData) of
        "" ->
            {true, Context#context{filename=template}};
        _ ->
            Tokens = wrq:path_tokens(ReqData),
            Filename = normalize_filepath(Tokens),
            {true, Context#context{filename=Filename}}
    end;
identify_resource(_ReqData, Context) ->
    {true, Context}.

%% @doc If the file exists, allow it through, otherwise assume true if
%%      they are asking for the application template.
-spec resource_exists(wrq:reqdata(), #context{}) ->
    {boolean(), wrq:reqdata(), #context{}}.
resource_exists(ReqData, Context) ->
    case identify_resource(ReqData, Context) of
        {true, NewContext=#context{filename=template}} ->
            {true, ReqData, NewContext};
        {true, NewContext=#context{filename=Filename}} ->
            case filelib:is_regular(Filename) of
                true ->
                    {true, ReqData, NewContext};
                _ ->
                    {false, ReqData, NewContext}
            end
    end.

%% @doc Return the proper content type of the file, or default to
%%      text/html.
-spec content_types_provided(wrq:reqdata(), #context{}) ->
    {list({list(), atom()}), wrq:reqdata(), #context{}}.
content_types_provided(ReqData, Context) ->
    case identify_resource(ReqData, Context) of
        {true, NewContext=#context{filename=template}} ->
            {[{"text/html", to_resource}], ReqData, NewContext};
        {true, NewContext=#context{filename=Filename}} ->
            MimeType = webmachine_util:guess_mime(Filename),
            {[{MimeType, to_resource}], ReqData, NewContext};
        {true, NewContext} ->
            {[{"text/html", to_resource}], ReqData, NewContext}
    end.

%% @doc Return the resources content.
-spec to_resource(wrq:reqdata(), #context{}) ->
    {binary(), wrq:reqdata(), #context{}}.
to_resource(ReqData, #context{filename=template}=Context) ->
    Token = tweeter_security:csrf_token(ReqData, Context),
    {ok, Content} = application_dtl:render([{csrf_token, Token}]),
    {Content,
     wrq:set_resp_header("Set-Cookie",
                         "csrf_token="++Token++"; httponly", ReqData),
     Context};
to_resource(ReqData, #context{filename=Filename}=Context) ->
    {ok, Source} = file:read_file(Filename),
    {Source, ReqData, Context}.

%% @doc Extract the priv dir for the application.
-spec priv_dir(term()) -> list().
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.
