-module(waychat_client).
-export([send_message/3]).

send_message(Host, Port, Message) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 2}]),
    ok = gen_tcp:send(Sock, Message),
    receive
        Data ->
            io:format("Data:~p", [Data])
    after 5000 ->
            io:format("Timeout")
    end,
    ok = gen_tcp:close(Sock).
