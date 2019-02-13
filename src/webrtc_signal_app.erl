%%%-------------------------------------------------------------------
%% @doc webrtc_signal public API
%% @end
%%%-------------------------------------------------------------------

-module(webrtc_signal_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_',[
          {'_', cowboy_static, {priv_file, webrtc_signal, "index.html"}}
        ]}
      ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], 
      #{ env => #{dispatch => Dispatch}}),
    webrtc_signal_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
