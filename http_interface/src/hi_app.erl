%%%-------------------------------------------------------------------
%%% @author Jakub Mitoraj <>
%%% @copyright (C) 2016, Jakub Mitoraj
%%% @doc
%%%
%%% @end
%%% Created : 23 Jan 2016 by Jakub Mitoraj <>
%%%-------------------------------------------------------------------
-module(hi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1156).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Port = case application:get_env(http_interface, port) of
	       {ok, P} -> P;
	       _ -> ?DEFAULT_PORT
	   end,
    IP = case application:get_env(http_interface, ip) of
	{ok, Ip} -> 
		 Ip;
	     _ -> undefined
	 end,
    case hi_sup:start_link(IP, Port) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
