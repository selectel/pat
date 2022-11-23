[![Build Status](https://github.com/selectel/pat/workflows/build/badge.svg)](https://github.com/selectel/pat)

    _
    |_) _ _|_
    |  (_| |_

        -- the only SMTP postman!

`pat` is an easy to use SMTP client for Erlang. You only need to remember
**two** functions:
* `pat:connect/2` takes an SMTP relay as a `{Host, Port}` pair and
  a list of supported options (see below) and opens an SMTP connection.
* `pat:send/2` sends a given email via an SMTP connection.

Example
-------

```erlang
(pat@postoffice)1> Opts = [{user, <<"pat">>}, {password, <<"postman">>}],
(pat@postoffice)2> Conn = pat:connect({<<"smtp.yandex.ru">>, 25}, Opts),
(pat@postoffice)3> rr("include/*").
(pat@postoffice)4> Email = #email{sender= <<"pat@ya.ru">>,
(pat@postoffice)4>                recipients=[<<"universe@gmail.com">>],
(pat@postoffice)4>                message= <<"Hello world!">>}.
(pat@postoffice)5> pat:send(Conn, Email).
{ok,<<"2.0.0 Ok: queued on smtp13.mail.yandex.net as mM6eOXwj-mM642Fvi">>}
```

Options
-------

#### `ssl`

**Type**: `boolean()`

**Description**: Connect to the SMTP server using a secure socket.

#### `tls`

**Type**: `never | maybe | always`

**Description**: Connect to the SMTP server via an unsecure socket and
    start TLS session afterward. `maybe` means TLS session will be sarted
    only if the relay supports `STARTTLS`.

#### `auth`

**Type**: `never | maybe | always`

**Description**: Authenticate with the SMTP server after connecting.
    When `maybe`, authentication is only performed if the server requires it.

#### `user` and `pasword`

**Type**: `binary()`

**Description**: The meaning is self explanatory. Should be used with `auth`.

#### `timeout`

**Type**: `timeout()`

**Description**: Sets a timeout (in seconds) for the underlying
  `gen_server`.
