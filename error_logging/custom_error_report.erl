%%%-------------------------------------------------------------------
%%% @author Jakub Mitoraj <jakub@jakub-Dell>
%%% @copyright (C) 2015, Jakub Mitoraj
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2015 by Jakub Mitoraj <jakub@jakub-Dell>
%%%-------------------------------------------------------------------
-module(custom_error_report).

-behaviour(gen_event).

%% API
-export([register_with_logger/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

register_with_logger() ->
    error_logger:add_report_handler(?MODULE).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------

handle_event({error, _Gleader, {Pid,Format,Data}}, State) ->
io:fwrite("ERROR: <~p> ~s", [Pid, io_lib:format(Format, Data)]),
{ok, State};
handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State) ->
io:fwrite("ERROR: <~p> ~p", [Pid, Report]),
{ok, State};
handle_event({error_report, _Gleader, {Pid, Type, Report}}, State) ->
io:fwrite("ERROR <~p> ~p ~p", [Pid, Type, Report]),
{ok, State};
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
io:fwrite("WARNING <~p> ~s", [Pid, io_lib:format(Format, Data)]),
{ok, State};
handle_event({warning_report,_Gleader,{Pid,std_warning,Report}}, State) ->
io:fwrite("WARNING <~p> ~p", [Pid, Report]),
{ok, State};
handle_event({warning_report,_Gleader,{Pid, Type, Report}}, State) ->
io:fwrite("WARNING <~p> ~p ~p", [Pid, Type, Report]),
{ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
io:fwrite("INFO <~p> ~s", [Pid, io_lib:format(Format, Data)]),
{ok, State};
handle_event({info_report, _Gleader, {Pid, std_info, Report}}, State) ->
io:fwrite("INFO <~p> ~p", [Pid, Report]),
{ok, State};
handle_event({info_report, _Gleader, {Pid, Type, Report}}, State) ->
io:fwrite("INFO <~p> ~p ~p", [Pid, Type, Report]),
{ok, State};
handle_event(_Event, State) ->
{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
