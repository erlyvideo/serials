-module(serials_app).

-behaviour(application).

%% API
-export([start/0, get_config/2]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API
%% ===================================================================
%% @doc Launcher for erl .. -s serials_app ..
start() ->
    start_lager(),
    application:start(serials).

%% @doc Get value or default from config
-spec get_config(atom(), term()) -> term().
get_config(Par, Default) ->
    case application:get_env(serials, Par) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    load_config(serials, "priv/serials.conf"),
    serials_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================
load_config(App, Config) ->
    {ok, Conf} = file:consult(Config),
    [application:set_env(App, K, V) || {K, V} <- Conf].

start_lager() ->
    application:load(lager),
    load_config(lager, "priv/lager.conf"),
    lager:start().
