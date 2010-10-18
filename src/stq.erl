%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(stq).

-include("stq.hrl").

%% Accessors
-export([type/1]).
-export([method/1]).
-export([code/1]).
-export([uri/1]).
-export([resp_msg/1]).
-export([vsn/1]).
-export([header_fields/1]).
-export([headers/1, headers/2]).
-export([body/1, body/2]).
-export([header/2]).
-export([header/4]).

%% API
-export([new/3]).

%% Internal Records
-record( stq_req, { method, uri, vsn, headers, body }).
-record( stq_res, { code, resp_msg, vsn, headers, body }).

%% -----------------------------------------------------------------------------
%% Accessors
%% -----------------------------------------------------------------------------
-spec type(stq_opaque()) -> request | response.
type(#stq_req{}) -> request;
type(#stq_res{}) -> response.

-spec method(stq_opaque()) -> sip_method(). 
method(#stq_req{ method = Value }) -> Value.

-spec code(stq_opaque()) -> sip_code(). 
code(#stq_res{ code = Value }) -> Value.

-spec uri(stq_opaque()) -> sip_uri(). 
uri(#stq_req{ uri = Value }) -> Value.

-spec resp_msg(stq_opaque()) -> sip_resp_msg(). 
resp_msg(#stq_res{ resp_msg = Value }) -> Value.

-spec vsn(stq_opaque()) -> sip_vsn(). 
vsn(#stq_req{ vsn = Value }) -> Value;
vsn(#stq_res{ vsn = Value }) -> Value.

-spec headers(stq_opaque()) -> list(sip_header()). 
headers(#stq_req{ headers = Value }) -> Value;
headers(#stq_res{ headers = Value }) -> Value.

-spec headers(list(sip_header()), stq_opaque()) -> stq_opaque().
headers(Value, #stq_req{ } = StqReq) -> StqReq#stq_req{ headers = Value };
headers(Value, #stq_res{ } = StqRes) -> StqRes#stq_res{ headers = Value }.

-spec body(stq_opaque()) -> sip_body(). 
body(#stq_req{ body = Value }) -> Value;
body(#stq_res{ body = Value }) -> Value.

-spec body(sip_body(), stq_opaque()) -> stq_opaque().
body(Value, #stq_req{ } = StqReq) -> StqReq#stq_req{ body = Value };
body(Value, #stq_res{ } = StqRes) -> StqRes#stq_res{ body = Value }.

-spec header_fields( stq_opaque() ) -> list(sip_header_field()).
header_fields(#stq_req{ headers = Headers }) ->
    [Field || {Field, _Value} <- Headers];
header_fields(#stq_res{ headers = Headers }) ->
    [Field || {Field, _Value} <- Headers].

-spec header( sip_header_field(), stq_opaque()) ->
    list({sip_header_value(), sip_header_line_no()}).
header(Header, #stq_req{ headers = Headers }) ->
    proplists:get_value(Header, Headers);
header(Header, #stq_res{ headers = Headers }) ->
    proplists:get_value(Header, Headers).

-spec header( sip_header_field(), sip_header_value(), sip_header_line_no(),
	      stq_opaque()) -> stq_opaque().
header(Header, Value, LnNo, #stq_req{ headers = Headers } = Stq) ->
    NewHeaders = insert_header(Header, {Value, LnNo}, Headers),
    Stq#stq_req{ headers = NewHeaders };
header(Header, Value, LnNo, #stq_res{ headers = Headers } = Stq) ->
    NewHeaders = insert_header(Header, {Value, LnNo}, Headers),
    Stq#stq_res{ headers = NewHeaders }.

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec new(sip_code() | sip_method(), sip_uri() | sip_resp_msg(), sip_vsn()) ->
    stq_opaque().
new(Code, Msg, Vsn) when is_integer(Code) ->
    #stq_res{ code = Code, resp_msg = Msg, vsn = Vsn, headers = [] };
new(Method, Uri, Vsn) ->
    #stq_req{ method = Method, uri = Uri, vsn = Vsn, headers = []  }.


%% -----------------------------------------------------------------------------
%% Internal Functions
%% -----------------------------------------------------------------------------
insert_header(Header, Value, [{Header, Values} | Rest]) ->
    [{Header, lists:reverse([Value | lists:reverse(Values)])} | Rest];
insert_header(Header, Value, [HeaderValue | Rest]) ->
    [HeaderValue | insert_header(Header, Value, Rest)];
insert_header(Header, Value, []) ->
    [{Header, [Value]}].
