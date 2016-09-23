#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name erl@localhost.localdomain -setcookie cookie

-mode(compile).

loop(N) ->
    receive
        A = {Pid, Ref, Port, Tuple, Msg} ->
            io:format("Received ~p~n", [A]),
            Pid ! {Pid, Ref, Port, Tuple, self(), make_ref(), [1|2], <<>>, <<"X">>, <<256:16>>, #{}, #{key => "value"}, 3.14, lists:concat(["echo: ", Msg, " (", N, ")"])};
        {Pid, Other} -> Pid ! Other
    end,
    loop(N + 1).

main(_) ->
    register(echo, self()),
    loop(0).
