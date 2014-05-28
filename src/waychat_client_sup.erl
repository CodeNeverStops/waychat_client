-module(waychat_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Host, Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Host, Port]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(waychat_client_worker, worker, [Host, Port])]} }.

