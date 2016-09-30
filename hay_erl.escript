#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name hay@localhost.localdomain -setcookie cookie

-mode(compile).

loop() ->
    receive
        {RemotePid, Value} -> RemotePid ! (Value + 1),
        loop();
        _ -> ok
    end.

main(_) ->
    register(hay, self()),
    loop().
