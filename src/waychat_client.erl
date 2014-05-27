-module(waychat_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {socket}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, user_register/2, user_login/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

user_register(UserName, Password) ->
    gen_server:call({local, ?SERVER}, {cmd, user_register, {UserName, Password}}).

user_login(UserName, Password) ->
    gen_server:call({local, ?SERVER}, {cmd, user_login, {UserName, Password}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Host, Port]) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 2}]),
    {ok, #state{socket = Sock}}.

handle_call({cmd, CMD, Param}, _From, State) ->
    gen_server:call({local, ?SERVER}, {send_message, {CMD, Param}}),
    {reply, ok, State};
handle_call({send_message, Message}, _From, #state{socket=Sock} = State) ->
    ok = gen_tcp:send(Sock, Message),
    receive
        Data ->
            io:format("Data:~p", [Data])
    after 5000 ->
            io:format("Timeout")
    end,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket=Sock}) ->
    ok = gen_tcp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

