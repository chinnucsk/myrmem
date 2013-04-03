%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) Erlware, LLC.
%%% @doc
%%% @end
-module(mmm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    case erlm_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
                end.

stop(_State) ->
    ok.
