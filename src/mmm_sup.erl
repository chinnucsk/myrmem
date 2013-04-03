%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) Erlware, LLC.
-module(mmm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    CowboyListener = {cowboy, {cowboy, start_listener,
                               [http, 100, cowboy_tcp_transport,
                                [{port, 8080}],
                                cowboy_http_protocol,
                                [{dispatch, erlm_routes:routes()}]]},
                      Restart, Shutdown, Type, [cowboy]},

    {ok, {SupFlags, [CowboyListener]}}.
