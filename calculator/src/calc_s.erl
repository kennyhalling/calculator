-module(calc_s).
-export([start/0, loop/1]).

start() ->
    spawn(?MODULE, loop, [[]]).

loop(History) ->
    receive
        {From, {add, X, Y}} ->
            Result = X + Y,
            Entry = io_lib:format("~p + ~p = ~p", [X, Y, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, {subtract, X, Y}} ->
            Result = X - Y,
            Entry = io_lib:format("~p - ~p = ~p", [X, Y, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, {multiply, X, Y}} ->
            Result = X * Y,
            Entry = io_lib:format("~p x ~p = ~p", [X, Y, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, {divide, X, 0}} ->
            Error = "Error: Divided by zero.",
            io:format("~s~n", [Error]),
            From ! {error, Error},
            loop([Error | History]);
        
        {From, {divide, X, Y}} ->
            Result = X / Y,
            Entry = io_lib:format("~p / ~p = ~p", [X, Y, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, {square, Base}} ->
            Result = math:pow(Base, 2),
            Entry = io_lib:format("~p ^ 2 = ~p", [Base, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, {power, Base, Exp}} when is_number(Base), is_number(Exp)->
            Result = math:pow(Base, Exp),
            Entry = io_lib:format("~p ^ ~p = ~p", [Base, Exp, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, {sqrt, X}} when X < 0 ->
            Error = "Error: Cannot take square root of negative number.",
            io:format("~s~n", [Error]),
            From ! {error, Error},
            loop([Error | History]);

        {From, {sqrt, X}} ->
            Result = math:sqrt(X),
            Entry = io_lib:format("sqrt(~p) = ~p", [X, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, {ln, X}} when X =< 0 ->
            Error = "Error: Cannot take log of non-positive number.",
            io:format("~s~n", [Error]),
            From ! {error, Error},
            loop([Error | History]);

        {From, {ln, X}} ->
            Result = math:log(X),
            Entry = io_lib:format("ln(~p) = ~p", [X, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, {log10, X}} when X =< 0 ->
            Error = "Error: Cannot take log10 of non-positive number.",
            io:format("~s~n", [Error]),
            From ! {error, Error},
            loop([Error | History]);

        {From, {log10, X}} ->
            Result = math:log10(X),
            Entry = io_lib:format("log10(~p) = ~p", [X, Result]),
            NewHistory = [lists:flatten(Entry) | History],
            io:format("~s~n", [lists:flatten(Entry)]),
            From ! {ok, Result},
            loop(NewHistory);

        {From, show_history} ->
            io:format("Calculation History:~n"),
            lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end, lists:reverse(History)),
            From ! ok,
            loop(History);

        {From, clear_history} ->
            io:format("History cleared.~n"),
            From ! ok,
            loop([]);

        {stop} ->
            io:format("Calculator stopped.~n"),
        ok;

        _Other ->
            io:format("Unknown message received.~n"),
            loop(History)
    end.