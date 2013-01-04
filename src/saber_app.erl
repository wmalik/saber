-module(saber_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(saber).

start(_StartType, StartArgs) ->
    DefaultConfigFile = hd(StartArgs),
    saber_sup:start_link([DefaultConfigFile]).

stop(_State) ->
    ok.
