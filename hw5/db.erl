% Programming Languages, COP 4020
% Fall 2015
% Homework #5
% Written by Alan Yepez

-module(db).
-export([start/0,stop/0,retrieve/1,insert/2]).

start() ->
    register(db, spawn(fun() ->
              loop() 
          end)
        ),
    {started}.
    
insert(Key, Value) ->
    rpc({insert, Key, Value}).

retrieve(Key) ->
    rpc({retrieve, Key}).

stop() ->
    rpc({stop}).

rpc(Request) ->
    db ! {self(), Request},
    receive
    {db, Reply} ->
        Reply
    end.

loop() ->
    receive
        {From,{insert, Key, Value}} ->
            put(Key, Value),
            From ! {db, done},
            loop();
        {From, {retrieve, Key}} ->
            From ! {db, get(Key)},
            loop();
        {From, {stop}} -> 
            From ! {db, stopped}
    end.