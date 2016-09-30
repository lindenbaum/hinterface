#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name erl2@localhost.localdomain -setcookie cookie

-mode(compile).

loop(N = 1000000) ->
    {hay, 'hay@localhost.localdomain'} ! ok,
    N;
loop(N) ->
    {hay, 'hay@localhost.localdomain'} ! {self(), N},
    receive
        A -> A
    end,
    loop(N + 1).

main(_) ->
    io:format("~p~n", [loop(0)]).
