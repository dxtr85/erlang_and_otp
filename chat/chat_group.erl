%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_group).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [foreach/2, reverse/2]).

-export([start/2]).

start(C, Nick) ->
    process_flag(trap_exit, true),
    controller(C, self()),
    send(C, ack),
    self() ! {chan, C, {relay, Nick, "I'm starting the group"}},
    group_controller([{C,Nick}]).



delete(Pid, [{Pid,Nick}|T], L) -> {Nick, reverse(T, L)};
delete(Pid, [H|T], L)          -> delete(Pid, T, [H|L]);
delete(_, [], L)               -> {"????", L}.



group_controller([]) ->
    exit(allGone);
group_controller(L) ->
    receive
	{chan, C, {relay, Nick, Str}} ->
	    foreach(fun({Pid,_}) -> send(Pid, {msg,Nick,C,Str})
		    end, L),
	    group_controller(L);
	{chan, C, {state, Pos, Value}} ->
	    io:format("Dostalem updateState w group ctrl\n",[]),
	    foreach(fun({Pid,_}) ->send(Pid,{updateState,Pos,Value})
	            end,L),
	    group_controller(L);
	{login, C, Nick} ->
	    controller(C, self()),
	    send(C, ack),
	    self() ! {chan, C, {relay, Nick, "I'm joining the group"}},
	    L1 = [{C,Nick}|L],
    	    self() ! {chan, C, {state,2,get_nicks(L1)}},
	    group_controller(L1);
	{chan_closed, C} ->
	    {Nick, L1} = delete(C, L, []),
	    self() ! {chan, C, {relay, Nick, "I'm leaving the group"}},
	    Nicks = get_nicks(L1),
	    self() ! {chan, C, {state,2,Nicks}},
	    io:format("Pozostali w grupie: ~p~n",[Nicks]),
	    group_controller(L1);
	Any ->
	    io:format("group controller received Msg=~p~n", [Any]),
	    group_controller(L)
    end.

get_nicks([])->
    [];
get_nicks([{_Chan,Nick}|T]) ->
    [Nick|get_nicks(T)].
