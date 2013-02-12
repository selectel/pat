%%% @author Sergei Levedev <superbobry@gmail.com>
%%%
%%% @copyright 2012 Selectel Ltd.
%%%
%%% @doc The only SMTP postman, which happens to know Erlang.
%%% @reference <a href="http://tools.ietf.org/html/rfc2821">RFC2821</a>:
%%%            Simple Mail Transfer Protocol
%%% @reference <a href="http://tools.ietf.org/html/rfc4954">RFC4954</a>:
%%%            SMTP Service Extension for Authentication
%%%
%%% @end

-module(pat).
-behaviour(application).

-include("pat.hrl").

%% API
-export([start/0, stop/0, connect/2, check/1, send/2]).

%% Application callbacks
-export([start/2, stop/1]).

%% Types

-export_type([relay/0, address/0, receipt/0]).
-export_type([error/0]).
-export_type([option/0, options/0]).

-type error() :: pat_smtp:error().

-type relay()   :: {binary(), inet:port_number()}.
-type address() :: binary().

-type receipt() :: pat_smtp:receipt().

-type option() :: {ssl, true | false}
                | {tls, never | always | maybe}
                | {hostname, binary()}
                | {user, binary()}
                | {password, binary()}
                | {timeout, timeout()}.
-type options() :: [option()].

-opaque connection() :: {pid(), options()}.

-export_type([connection/0]).

%% API

start() ->
    lager:start(),
    lager:set_loglevel(lager_console_backend, debug),

    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(gproc),
    application:start(pat).

stop() ->
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(gproc),
    application:stop(pat).

%%% @doc Connects to a given STMP relay and returns an SMTP connection.
%%%      Note, that a relay workers are reused between @{link connect/2}
%%%      calls.
%%% @end
-spec connect(relay(), options()) -> connection().
connect({Host, Port}=Relay, Opts)
  when is_binary(Host) andalso is_integer(Port) ->
    Pid = pat_manager:lookup_relay(Relay),
    {Pid, Opts}.

%%% @doc Checks that a conection is valid, i. e. that we can establish
%%%      an SMTP session and authenticate using configure credentials
%%%      (if any).
%%% @end
-spec check(connection()) -> {ok, binary()} | {error, error()}.
check({Pid, Opts}) when is_pid(Pid) ->
    pat_worker:check(Pid, Opts).

%%% @doc Sends an email via a given SMTP relay connection and returns
%%%      a receipt, if everything worked or an appropriate error atom.
%%% @end
-spec send(connection(), #email{}) -> {ok, receipt()} | {error, error()}.
send({Pid, Opts}, #email{sender= <<_/binary>>,
                         recipients=[<<_/binary>>|_],
                         message= <<_/binary>>}=Email)
  when is_pid(Pid) ->
    pat_worker:send(Pid, Email, Opts).

%% Application callbacks

start(_StartType, _StartArgs) ->
    pat_sup:start_link().

stop(_State) ->
    ok.
