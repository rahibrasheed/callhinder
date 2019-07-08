%%%-------------------------------------------------------------------
%% @doc callhinder public API
%% @end
%%%-------------------------------------------------------------------

-module(callhinder_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                      {'_', [{"/",cowboy_static,{file,"www/index.html"}},
                             {"/callhinder/[...]", callhinder_api, []}]}
                      ]),
    cowboy:start_http(monitor_http_listener, 100, [{port, 8089}],
                            [{env, [{dispatch, Dispatch}]}]),
    callhinder_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
