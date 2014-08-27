     _
    |_) _ _|_
    |  (_| |_

        -- the only SMTP postman!

[![Build Status](https://travis-ci.org/selectel/pat.svg)](http://travis-ci.org/selectel/pat)

`pat` is an easy to use SMTP client for Erlang. You only need to know
**two** functions:
* `pat:connect/2`, which takes an SMTP relay, a `{Host, Port}` pair and
  a list of SMTP options, listed bellow and returns an SMTP connection.
* `pat:send/2`, which sends a given email via an SMTP connection.

Here's a quick example:

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

As the name suggest, all connection options are **optional**. Here's a list
of them:

```
| Option                        | Description                                     |
|-------------------------------+-------------------------------------------------|
| {ssl, true | false}           | Use SSL for connection.                         |
| {tls, never | always | maybe} | Start a TLS session, after connecting via an    |
|                               | unsecure socket, passing 'maybe' means: use TLS |
|                               | only if the relay supports STARTTLS command.    |
| {user, binary()}              | SMTP auth. username.                            |
| {password, binary()}          | SMTP auth. password.                            |
| {timeout, timeout()}          | A timeout in *seconds* for the underlying       |
|                               | gen_server.                                     |
```
