-module(pat_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, lookup_relay/1]).

%% gen_qserver callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec lookup_relay(pat:relay()) -> pid().
lookup_relay(Relay) ->
    gen_server:call(?SERVER, {lookup_relay, Relay}).

%%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({lookup_relay, Relay}, _From, State) ->
    Key   = {n, l, {pat_worker, Relay}},
    Reply =
        case gproc:where(Key) of
            undefined ->
                {ok, Pid} = supervisor:start_child(pat_workers_sup, [Relay]),
                gproc:await(Key),
                Pid;
            Pid when is_pid(Pid) ->
                Pid
        end,
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    error_logger:warning_msg("Got unhandled call: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("Got unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:warning_msg("Got unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
