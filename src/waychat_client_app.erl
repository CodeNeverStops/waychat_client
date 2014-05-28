-module(waychat_client_app).

-behaviour(application).
-define(HOST, localhost).
-define(PORT, 9999).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ListenHost = get_app_env(listen_host, ?HOST),
    ListenPort = get_app_env(listen_port, ?PORT),
    waychat_client_sup:start_link(ListenHost, ListenPort).

stop(_State) ->
    ok.

get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of 
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error       -> Default
            end
    end.

