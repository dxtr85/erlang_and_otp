%% This is a simple moodule.
-module(my_module).
-export([pie/0,print/1,either_or_both/2,area/1,sign/1]).

-record(customer,{name="Anonymous",phone,address}).

pie()->
    3.14.

print(Term)->
    io:format("This is in Term: ~w~n",[Term]).
either_or_both(A,B)->
    case {A,B} of
	{true,B} when is_boolean(B)->
	    true;
	{A,true} when is_boolean(A) ->
	    true;
	{false,false} ->
	    false
    end.

area({circle,Radius})->
    Radius*Radius*math:pi();
area({square,Side}) ->
    Side*Side;
area({rectangle,Width,Height}) ->
    Width*Height.

sign(N)	when is_number(N)->
    if N > 0 ->
	    positive;
       N < 0 ->
	    negative;
       true-> zero
    end.


