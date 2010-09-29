%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(stq).

-include("stq.hrl").

-record( stq_msg, { method, uri, vsn, headers, body}).

-define(getter(Name), Name(#stq_msg{ Name = Value }) -> Value).
-define(setter(Name), Name(Value, StqMsg) -> StqMsg#stq_msg{ Name = Value }).

-export([new/3]).
-export([method/1, method/2]).
-export([uri/1, uri/2]).
-export([vsn/1, vsn/2]).
-export([header_fields/1]).
-export([headers/1, headers/2]).
-export([body/1, body/2]).
-export([header/2]).

new(Method, Uri, Vsn) ->
    #stq_msg{ method = Method, uri = Uri, vsn = Vsn }.

?getter(method).
?setter(method).
?getter(uri).
?setter(uri).
?getter(vsn).
?setter(vsn).
?getter(headers).
?setter(headers).
?getter(body).
?setter(body).

header_fields(#stq_msg{ headers = Headers }) ->
    [Field || {Field, _Value} <- Headers].

header(Header, #stq_msg{ headers = Headers }) ->
    proplists:get_value(Header, Headers).
