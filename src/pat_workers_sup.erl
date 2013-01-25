-module(pat_workers_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    Type = simple_one_for_one,
    MaxRestarts = 5,
    MaxSeconds  = 10,
    Module = pat_worker,
    TypeSup = {Module,
               {Module, start_link, []},
               permanent,
               5000,
               supervisor,
               [Module]},
    {ok, {{Type, MaxRestarts, MaxSeconds}, [TypeSup]}}.
