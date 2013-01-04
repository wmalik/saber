
-module(saber_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD_ARGS(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(DefaultConfig) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [DefaultConfig]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([DefaultConfig]) ->
    {ok, {
            {one_for_one, 5, 10},
            [?CHILD_ARGS(saber_api, worker, DefaultConfig)]
         }
    }.
