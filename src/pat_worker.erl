-module(pat_worker).
-behaviour(gen_server).

-include("pat.hrl").

%% API
-export([start_link/1, send/3, check/2, wrap/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {relay :: pat:relay()}).

%%% API

start_link(Relay) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Relay], []).

-spec check(pid(), pat:options())
           -> {ok, binary()} | {error, pat_smtp:error()}.
check(Pid, Opts) ->
    gen_server:call(Pid, {check, Opts}, infinity).

-spec send(pid(), #email{}, pat:options())
          -> {ok, pat:receipt()} | {error, pat_smtp:error()}.
send(Pid, Email, Opts) ->
    gen_server:call(Pid, {send, Email, Opts}, infinity).

%%% gen_server callbacks

init([Relay]) ->
    self() ! register,
    {ok, #state{relay=Relay}}.

handle_call({check, Opts}, _From, #state{relay=Relay}=State) ->
    {MXDomain, Port} = Relay,
    Reply = check_any(mxlookup(binary_to_list(MXDomain)), Port, Opts),
    {reply, Reply, State};
handle_call({send, Email, Opts}, _From, #state{relay=Relay}=State) ->
    {MXDomain, Port} = Relay,
    Reply = send_any(mxlookup(binary_to_list(MXDomain)), Port,
                     wrap(Email), Opts),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(register, #state{relay=Relay}=State) ->
    undefined = gproc:where({n, l, {pat_worker, Relay}}),
    gproc:reg({n, l, {pat_worker, Relay}}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

-spec join([binary()], binary()) -> binary().
join([], _Sep)   -> <<>>;
join([H], _Sep)  -> H;
join([H|T], Sep) -> <<H/binary, Sep/binary, (join(T, Sep))/binary>>.

-spec wrap(#email{}) -> pat_smtp:envelope().
wrap(Email) ->
    {ok, Date} = tempo:format(rfc2822, {now, os:timestamp()}),
    Hdrs = [{<<"Date">>, Date},
            {<<"From">>, Email#email.sender},
            {<<"To">>, join(Email#email.recipients, <<", ">>)},
            {<<"Subject">>, Email#email.subject}|Email#email.headers],
    Meta = [<<Key/binary, ": ", Val/binary>> || {Key, Val} <- Hdrs],
    {Email#email.sender,
     Email#email.recipients,
     <<(join(Meta, <<"\r\n">>))/binary, "\r\n",
       "\r\n",
       (Email#email.message)/binary>>}.

%% @doc Returns a sorted list of MX servers for `Domain', lowest distance first.
-spec mxlookup(inet:hostname()) -> [{integer(), inet_res:dns_name()}].
mxlookup(Domain) ->
    case inet_res:lookup(Domain, in, mx) of
        %% Note(superbobry): maybe we're supposed to relay to a host
        %% directly.
        []           -> [{0, Domain}];
        [_|_]=Result ->
            lists:sort(fun({Prio1, _Name1}, {Prio2, _Name2}) ->
                               Prio1 =< Prio2
                       end, Result)
    end.

-spec check_any([{integer(), inet_res:dns_name()}],
                inet:port_number(),
                pat:options()) -> {ok, binary()}
                                | {error, pat_smtp:error()}.
check_any([{_Prio, Host}|Hosts], Port, Opts) ->
    case check_one(Host, Port, Opts) of
        {ok, Banner} -> {ok, Banner};
        {error, _Reason} when Hosts =/= [] ->
            check_any(Hosts, Port, Opts);
        {error, _Reason}=Error -> Error
    end.

-spec check_one(inet_res:dns_name(),
                inet:port_number(),
                pat:options()) -> {ok, binary()}
                                | {error, pat_smtp:error()}.
check_one(Host, Port, Opts) ->
    case pat_smtp:connect(Host, Port, Opts) of
        {ok, {Connection, Banner}} ->
            Result = case pat_smtp:check(Connection, Opts) of
                         ok -> {ok, Banner};
                         {error, _Reason}=Error -> Error
                     end,
            pat_smtp:close(Connection),
            Result;
        {error, _Reason}=Error -> Error
    end.

-spec send_any([{integer(), inet_res:dns_name()}],
               inet:port_number(),
               pat_smtp:envelope(),
               pat:options()) -> {ok, pat:receipt()}
                               | {error, pat_smtp:error()}.
send_any([{_Prio, Host}|Hosts], Port, Envelope, Opts) ->
    case send_one(Host, Port, Envelope, Opts) of
        {ok, Receipt} -> {ok, Receipt};
        {error, Reason} when
              %% Switch to another host on 4XX errors if any.
              (Reason =:= service_not_available orelse
               Reason =:= mailbox_unavailable orelse
               Reason =:= local_error orelse
               Reason =:= insufficient_system_storage)
              andalso Hosts =/= [] ->
            send_any(Hosts, Port, Envelope, Opts);
        {error, _Reason}=Error -> Error
    end.

-spec send_one(inet_res:dns_name(),
               inet:port_number(),
               pat_smtp:envelope(),
               pat:options()) -> {ok, pat:receipt()}
                               | {error, pat_smtp:error()}.
send_one(Host, Port, Envelope, Opts) ->
    case pat_smtp:connect(Host, Port, Opts) of
        {ok, {Connection, _Banner}} ->
            Result = pat_smtp:send(Connection, Envelope, Opts),
            pat_smtp:close(Connection),
            Result;
        {error, _Reason}=Error -> Error
    end.
