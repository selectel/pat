-module(pat_smtp).
-compile([{parse_transform, lager_transform}]).
-include_lib("kernel/include/inet.hrl").

%% Public API.
-export([connect/3, close/1, check/2, send/3]).

%% SMTP commands.
-export([helo/3, ehlo/3, starttls/2, auth/3,
         mail/3, rcpt/3, data/3, vrfy/3,
         rset/2, noop/2, quit/2]).

-export_type([connection/0, envelope/0, error/0]).

-type error() :: success
               | help
               | service_ready
               | service_closing
               | action_completed
               | service_not_available
               | mailbox_unavailable
               | local_error
               | unknown_command
               | invalid_arguments
               | not_implemnted
               | unexpected_command
               | access_denied
               | mailbox_unavailable
               | transaction_failed
               | authentication_failed
               | inet:posix().

-type status() :: pos_integer().
-type banner() :: binary().

%% FIXME(Sergei): 'ssl' doesn't export it's socket type, nor does 'gen_tcp';
%% what a nice example of a fucked-up encapsulation.
-record(sslsocket, {fd = nil, pid = nil}).

-type connection() :: {gen_tcp, inet:socket()} | {ssl, #sslsocket{}}.
-type envelope()   :: {pat:address(), [pat:address()], binary()}.
-type receipt()    :: binary().

%% Public API.

-spec connect(inet:hostname(), inet:port_number(), pat:options())
             -> {ok, {connection(), banner()}}
              | {error, error()}.
connect(Host, Port, Opts) ->
    ActualHost = case inet_parse:address(Host) of
                     {ok, IP} -> IP;
                     _        -> Host
                 end,
    M = case proplists:get_value(ssl, Opts, false) of
            true  -> ssl;
            false -> gen_tcp
        end,
    SocketOpts = [binary, {active, false}, {packet, line}],
    Timeout = proplists:get_value(timeout, Opts, infinity),
    case M:connect(ActualHost, Port, SocketOpts, Timeout) of
        {ok, Socket}           ->
            Connection = {M, Socket},
            Result = case recv(Connection, Timeout) of
                         {ok, [{220, Banner}|_]} -> {ok, {Connection, Banner}};
                         {ok, [{Status, _}|_]}   -> {error, code(Status)};
                         {error, _Reason}=Error  -> Error
                     end,
            case Result of
                {ok, _} -> ok;
                _       -> M:close(Socket)
            end,
            Result;
        {error, _Reason}=Error -> Error
    end.

-spec close(connection()) -> ok.
close({M, Socket}) ->
    M:close(Socket).

-spec check(connection(), pat:options()) -> ok | {error, error()}.
check(Connection, Opts) ->
    case do_check(Connection, Opts) of
        {ok, _} -> ok;
        {error, _Reason}=Error -> Error
    end.

-spec send(connection(), envelope(), pat:options())
          -> {ok, receipt()} | {error, error()}.
send(Connection, Envelope, Opts) ->
    case do_check(Connection, Opts) of
        {ok, NewConnection} ->
            do_send(NewConnection, Envelope, Opts);
        {error, _Reason}=Error -> Error
    end.

%% Helpers.

do_check(Connection, Opts) ->
    FQDN = case proplists:get_value(hostname, Opts) of
               Hostname when is_binary(Hostname) -> Hostname;
               undefined -> fqdn()
           end,
    Timeout = proplists:get_value(timeout, Opts, infinity),
    {ok, Extensions} = do_ehlo(Connection, FQDN, Timeout),
    {NewConnection, NewExtensions} =
        do_starttls(Connection, Extensions, [{hostname, FQDN}|Opts]),
    case do_auth(NewConnection, NewExtensions, Opts) of
        {ok, true} -> {ok, NewConnection};
        {ok, false} -> {error, authentication_failed};
        {error, _Reason}=Error -> Error
    end.

do_ehlo(Connection, FQDN, Timeout) ->
    case ehlo(Connection, FQDN, Timeout) of
        {error, unknown_command} -> helo(Connection, FQDN, Timeout);
        {_, _}=Result            -> Result
    end.

do_starttls(Connection, Extensions, Opts) ->
    Timeout   = proplists:get_value(timeout, Opts, infinity),
    Supported = lists:member(<<"STARTTLS">>, Extensions),
    case proplists:get_value(tls, Opts, maybe) of
        never -> {Connection, Extensions};
        maybe when not Supported -> {Connection, Extensions};
        _ when Supported ->
            {ok, NewConnection} = starttls(Connection, Timeout),
            {ok, NewExtensions} = do_ehlo(NewConnection,
                                          proplists:get_value(hostname, Opts),
                                          Timeout),
            {NewConnection, NewExtensions}
    end.

do_auth(Connection, Extensions, Opts) ->
    AuthTypes = proplists:get_value(<<"AUTH">>, Extensions, []),
    Timeout   = proplists:get_value(timeout, Opts, infinity),
    User      = proplists:get_value(user, Opts),
    Password  = proplists:get_value(password, Opts),
    HasUserPassword = User =/= undefined andalso Password =/= undefined,
    case proplists:get_value(auth, Opts, maybe) of
        always -> {ok, true};                 %% server without AUTH
        never -> {ok, undefined};
        maybe when not HasUserPassword ->
            case AuthTypes of
                [] -> {ok, undefined};        %% ok, forget it :)
                _  -> {error, missing_auth}   %% server requires AUTH!
            end;
        _ when HasUserPassword ->
            auth(Connection, {any, User, Password, AuthTypes}, Timeout)
    end.

do_send(Connection, {Sender, Recipients, Message}, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, infinity),
    mail(Connection, Sender, Timeout),
    rcpt(Connection, Recipients, Timeout),
    data(Connection, Message, Timeout).

%% SMTP commands.

-spec ehlo(connection(), binary(), timeout())
          -> {ok, [binary()]} | {error, error()}.
ehlo(Connection, FQDN, Timeout) when is_binary(FQDN) ->
    case communicate(Connection,
                     <<"EHLO ", FQDN/binary, "\r\n">>,
                     250, Timeout) of
        {ok, [_|Extensions]}   ->
            {ok, [case binary:split(Chunk, <<" ">>, [global]) of
                      [Name]       -> Name;
                      [Name|Value] -> {Name, Value}
                  end || Chunk <- Extensions]};
        {error, _Reason}=Error -> Error
    end.

-spec helo(connection(), binary(), timeout())
          -> ok | {error, error()}.
helo(Connection, FQDN, Timeout) when is_binary(FQDN)->
    case communicate(Connection,
                     <<"HELO ", FQDN/binary, "\r\n">>,
                     250, Timeout) of
        {ok, _}                -> ok;
        {error, _Reason}=Error -> Error
    end.

-spec starttls(connection(), timeout())
              -> {ok, connection()} | {error, error()}.
starttls({ssl, _Socket}, _Timeout) ->
    {error, already_ssl};
starttls({_, Socket}=Connection, Timeout) ->
    case communicate(Connection,
                     <<"STARTTLS\r\n">>,
                     220, Timeout) of
        {ok, _}                ->
            case ssl:connect(Socket, [], Timeout) of
                {ok, SslSocket}        -> {ok, {ssl, SslSocket}};
                {error, _Reason}=Error -> Error
            end;
        {error, _Reason}=Error -> Error
    end.

-spec auth(connection(),
             {login | plain, binary(), binary()}
           | {any, binary(), binary(), [binary()]},
           timeout()) -> {ok, boolean()} | {error, atom()}.
auth(Connection, {login, User, Password}, Timeout) ->
    ok = send(Connection, <<"AUTH LOGIN\r\n">>),
    case recv(Connection, Timeout) of
        {ok, [{334, <<"VXNlcm5hbWU6">>}]} ->
            ok = send(Connection, <<(base64:encode(User))/binary, "\r\n">>),
            case recv(Connection, Timeout) of
                {ok, [{334, <<"UGFzc3dvcmQ6">>}]} ->
                    ok = send(Connection,
                              <<(base64:encode(Password))/binary, "\r\n">>),
                    case recv(Connection, Timeout) of
                        {ok, [{Status, _}|_]}  -> {ok, Status =:= 235};
                        {error, _Reason}=Error -> Error
                    end;
                {ok, [_]}              -> {ok, false};
                {error, _Reason}=Error -> Error
            end;
        {ok, [_]}              -> {ok, false};
        {error, _Reason}=Error -> Error
    end;
auth(Connection, {plain, User, Password}, Timeout) ->
    String = base64:encode(<<0, User/binary, 0, Password/binary>>),
    ok     = send(Connection, <<"AUTH PLAIN ", String/binary, "\r\n">>),
    case recv(Connection, Timeout) of
        {ok, [{Status, _}|_]}  -> {ok, Status =:= 235};
        {error, _Reason}=Error -> Error
    end;
auth(_Connection, {any, _User, _Password, []}, _Timeout) ->
    {ok, false};
auth(Connection, {any, User, Password, [AuthType|AuthTypes]}, Timeout) ->
    Type = case AuthType of
               <<"LOGIN">>    -> login;
               <<"PLAIN">>    -> plain
           end,
    case auth(Connection, {Type, User, Password}, Timeout) of
        {ok, false} when AuthTypes =/= [] ->
            auth(Connection, {any, User, Password, AuthTypes}, Timeout);
        {_, _}=Result -> Result
    end.

-spec mail(connection(), pat:address(), timeout()) -> ok | {error, error()}.
mail(Connection, Address, Timeout) ->
    communicate(Connection,
                <<"MAIL FROM: ", (quoteaddr(Address))/binary, "\r\n">>,
                250, Timeout).

-spec rcpt(connection(), [pat:address()], timeout())
             -> ok | {error, atom()}.
rcpt(_Connection, [], _Timeout) ->
    ok;
rcpt(Connection, [Address|Addresses], Timeout) ->
    ok = send(Connection,
              <<"RCPT TO: ", (quoteaddr(Address))/binary, "\r\n">>),
    case recv(Connection, Timeout) of
        {ok, [{Status, _}]} when Status =:= 250; Status =:= 251 ->
            rcpt(Connection, Addresses, Timeout);
        {ok, [{Status, _}|_]} -> {error, code(Status)};
        {error, _Reason}=Error -> Error
    end.

-spec data(connection(), binary(), timeout())
          -> {ok, receipt()} | {error, error()}.
data(Connection, Message, Timeout) ->
    case communicate(Connection, <<"DATA\r\n">>, 354, Timeout) of
        {ok, _} ->
            case communicate(Connection,
                             <<Message/binary, "\r\n", ".\r\n">>,
                             250, Timeout) of
                {ok, [Receipt]}        -> {ok, Receipt};
                {error, _Reason}=Error -> Error
            end;
        {error, _Reason}=Error -> Error
    end.

-spec vrfy(connection(), pat:address(), timeout())
          -> boolean() | {error, error()}.
vrfy(Connection, Address, Timeout) ->
    case communicate(Connection,
                     <<"VRFY ", (quoteaddr(Address))/binary, "\r\n">>,
                     250, Timeout) of
        {ok, [_]}               -> true;
        {error, user_not_local} -> false;
        {error, vrfy_failed}    -> false;
        {error, _Reason}=Error  -> Error
    end.

-spec quit(connection(), timeout()) -> ok | {error, error()}.
quit(Connection, Timeout) ->
    case communicate(Connection, <<"QUIT\r\n">>, 221, Timeout) of
        {ok, _}                -> ok;
        {error, _Reason}=Error -> Error
    end.

-spec rset(connection(), timeout()) -> ok | {error, error()}.
rset(Connection, Timeout) ->
    case communicate(Connection, <<"RSET\r\n">>, 250, Timeout) of
        {ok, _}                -> ok;
        {error, _Reason}=Error -> Error
    end.

-spec noop(connection(), timeout()) -> ok | {error, error()}.
noop(Connection, Timeout) ->
    case communicate(Connection, <<"NOOP\r\n">>, 250, Timeout) of
        {ok, _}                -> ok;
        {error, _Reason}=Error -> Error
    end.

%% Low-level API.

-spec communicate(connection(), binary(), status(), timeout())
                 -> {ok, [binary()]} | {error, error()}.
communicate(Connection, Command, Status, Timeout) ->
    ok = send(Connection, Command),
    case recv(Connection, Timeout) of
        {ok, [{Status, _}|_]=Chunks} ->
            {ok, [Line || {_, Line} <- Chunks]};
        {ok, [{ActualStatus, _}|_]}  -> {error, code(ActualStatus)};
        {error, _Reason}=Error       -> Error
    end.

-spec send(connection(), binary()) -> ok | {error, inet:posix()}.
send({M, Socket}, Data) ->
    lager:debug("~s", [Data]),
    M:send(Socket, Data).

-spec recv(connection(), timeout())
          -> {ok, [{status(), binary()}]} | {error, inet:posix()}.
recv(Connection, Timeout) ->
    recv(Connection, Timeout, []).

recv({M, Socket}=Connection, Timeout, Acc) ->
    case M:recv(Socket, 0, Timeout) of
        {ok, Data= <<Status:3/binary, Sep:1/binary, Rest/binary>>} ->
            lager:debug("~s", [Data]),
            NewAcc = [{list_to_integer(binary_to_list(Status)),
                       binary:replace(Rest, [<<"\r">>, <<"\n">>],
                                      <<"">>, [global])}
                      |Acc],
            case Sep of
                <<"-">> -> recv(Connection, Timeout, NewAcc);
                <<" ">> -> {ok, lists:reverse(NewAcc)}
            end;
        {error, _Reason}=Error -> Error
    end.

-spec code(status()) -> error().
code(200) -> success;
code(211) -> help;
code(220) -> service_ready;
code(221) -> service_closing;
code(250) -> action_completed;
code(251) -> user_not_local;
code(252) -> vrfy_failed;
code(421) -> service_not_available;
code(450) -> mailbox_unavailable;
code(451) -> local_error;
code(452) -> insufficient_system_storage;
code(500) -> unknown_command;
code(501) -> invalid_arguments;
code(502) -> not_implemnted;
code(503) -> unexpected_command;
code(530) -> access_denied;
code(535) -> authentication_failed;
code(550) -> mailbox_unavailable;
code(551) -> user_not_local;
code(553) -> mailbox_syntax_incorrect;
code(554) -> transaction_failed.

-spec fqdn() -> binary().
fqdn() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, Hostent}  = inet:gethostbyname(Hostname),
    list_to_binary(Hostent#hostent.h_name).

-spec quoteaddr(pat:address()) -> pat:address().
quoteaddr(<<"">>) ->
    <<"<>">>;
quoteaddr(Address) when is_binary(Address) ->
    %% This is by no means a complete RFC2822 implementation -- just
    %% a quick way to fix the address, in case it's not wrapped in
    %% angle brackets.
    ButLast = byte_size(Address) - 1,
    case Address of
        <<_:ButLast/binary, ">">> -> Address;
        _                         -> <<"<", Address/binary, ">">>
    end.
