%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(esessin_header).

-include("stq.hrl").

%% General header line decoder
-export([decode/5]).
-export([default_hooks/0]).
-export([no_hooks/0]).

%% Header specific decoders
-export(['Accept'/1]).
-export(['Accept-Encoding'/1]).
-export(['Alert-Info'/1]).
-export(['Allow'/1]).
-export(['Authentication-Info'/1]).
-export(['Authorization'/1]).
-export(['Call-Id'/1]).
-export(['Call-Info'/1]).
-export(['Contact'/1]).
-export(['Content-Disposition'/1]).
-export(['Content-Encoding'/1]).
-export(['Content-Language'/1]).
-export(['Content-Type'/1]).
-export(['Cseq'/1]).
-export(['Date'/1]).
-export(['Error-Info'/1]).
-export(['Expires'/1]).
-export(['From'/1]).
-export(['In-Reply-To'/1]).
-export(['Max-Forwards'/1]).
-export(['Min-Expires'/1]).
-export(['Mime-Version'/1]).
-export(['Organization'/1]).
-export(['Priority'/1]).
-export(['Proxy-Authenticate'/1]).
-export(['Proxy-Authorization'/1]).
-export(['Proxy-Require'/1]).
-export(['Record-Route'/1]).
-export(['Reply-To'/1]).
-export(['Require'/1]).
-export(['Retry-After'/1]).
-export(['Route'/1]).
-export(['Server'/1]).
-export(['Subject'/1]).
-export(['Supported'/1]).
-export(['Timestamp'/1]).
-export(['To'/1]).
-export(['Unsupported'/1]).
-export(['User-Agent'/1]).
-export(['Via'/1]).
-export(['Warning'/1]).
-export(['Www-Authenticate'/1]).
-export([default_header/1]).


%% --------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------
-spec decode(sip_header_field(), sip_header_value(), sip_header_line_no(),
	     stq_opaque(), HeaderHooks :: dict()) ->
    stq_opaque().
decode(Type, Value, LnNo, Opaque, HeaderHooks)
  when Type =:= 'Www-Authenticate';
       Type =:= 'Authorization';
       Type =:= 'Proxy-Authenticate';
       Type =:= 'Proxy-Authorization' ->
    %% For some strange reason these four should not be split on comma's
    %% See rfc 3261 § 7.3.1
    parse_value(Type, Value, LnNo, Opaque, HeaderHooks);
decode(Type, Values, LnNo, Opaque, HeaderHooks) ->
    case binary:split(Values, <<",">>) of
	[Value, Rest] ->
	    NewOpaque = parse_value(Type, Value, LnNo, Opaque, HeaderHooks),
	    decode(Type, Rest, incr(LnNo), NewOpaque, HeaderHooks);
	[Value] ->
	    parse_value(Type, Value, LnNo, Opaque, HeaderHooks)
    end.

-spec default_hooks() -> dict().
default_hooks() ->
    dict:from_list([{'Accept', fun 'Accept'/1},
		    {'Accept-Encoding', fun 'Accept-Encoding'/1},
		    {'Alert-Info', fun 'Alert-Info'/1},
		    {'Allow', fun 'Allow'/1},
		    {'Authentication-Info', fun 'Authentication-Info'/1},
		    {'Authorization', fun 'Authorization'/1},
		    {'Call-Id', fun 'Call-Id'/1},
		    {'Call-Info', fun 'Call-Info'/1},
		    {'Contact', fun 'Contact'/1},
		    {'Content-Disposition', fun 'Content-Disposition'/1},
		    {'Content-Encoding', fun 'Content-Encoding'/1},
		    {'Content-Length', fun 'Content-Length'/1},
		    {'Content-Language', fun 'Content-Language'/1},
		    {'Content-Type', fun 'Content-Type'/1},
		    {'Cseq', fun 'Cseq'/1},
		    {'Date', fun 'Date'/1},
		    {'Error-Info', fun 'Error-Info'/1},
		    {'Expires', fun 'Expires'/1},
		    {'From', fun 'From'/1},
		    {'In-Reply-To', fun 'In-Reply-To'/1},
		    {'Max-Forwards', fun 'Max-Forwards'/1},
		    {'Min-Expires', fun 'Min-Expires'/1},
		    {'Mime-Version', fun 'Mime-Version'/1},
		    {'Organization', fun 'Organization'/1},
		    {'Priority', fun 'Priority'/1},
		    {'Proxy-Authenticate', fun 'Proxy-Authenticate'/1},
		    {'Proxy-Authorization', fun 'Proxy-Authorization'/1},
		    {'Proxy-Require', fun 'Proxy-Require'/1},
		    {'Record-Route', fun 'Record-Route'/1},
		    {'Reply-To', fun 'Reply-To'/1},
		    {'Require', fun 'Require'/1},
		    {'Retry-After', fun 'Retry-After'/1},
		    {'Route', fun 'Route'/1},
		    {'Server', fun 'Server'/1},
		    {'Subject', fun 'Subject'/1},
		    {'Supported', fun 'Supported'/1},
		    {'Timestamp', fun 'Timestamp'/1},
		    {'To', fun 'To'/1},
		    {'Unsupported', fun 'Unsupported'/1},
		    {'User-Agent', fun 'User-Agent'/1},
		    {'Via', fun 'Via'/1},
		    {'Warning', fun 'Warning'/1},
		    {'Www-Authenticate', fun 'Www-Authenticate'/1},
		    {default, fun default_header/1}]).

-spec no_hooks() -> dict().
no_hooks() ->
    %% We have to have Content-Length here, otherwise body fetching will fail
    dict:from_list([{'Content-Length', fun 'Content-Length'/1},
		    {default, fun(Value) -> Value end}]).

%% --------------------------------------------------------------------------
%% Header specific decoders
%% --------------------------------------------------------------------------

-spec 'Accept'(sip_header_value()) -> term().
'Accept'(Value) -> default_header(Value).

-spec 'Accept-Encoding'(sip_header_value()) -> term().
'Accept-Encoding'(Value) -> default_header(Value).

-spec 'Alert-Info'(sip_header_value()) -> term().
'Alert-Info'(Value) -> default_header(Value).

-spec 'Allow'(sip_header_value()) -> term().
'Allow'(Value) -> default_header(Value).

-spec 'Authentication-Info'(sip_header_value()) -> term().
'Authentication-Info'(Value) -> default_header(Value).

-spec 'Authorization'(sip_header_value()) -> term().
'Authorization'(Value) -> default_header(Value).

-spec 'Call-Id'(sip_header_value()) -> term().
'Call-Id'(Value) -> default_header(Value).

-spec 'Call-Info'(sip_header_value()) -> term().
'Call-Info'(Value) -> default_header(Value).

-spec 'Contact'(sip_header_value()) -> term().
'Contact'(Value) -> default_header(Value).

-spec 'Content-Disposition'(sip_header_value()) -> term().
'Content-Disposition'(Value) -> default_header(Value).

-spec 'Content-Encoding'(sip_header_value()) -> term().
'Content-Encoding'(Value) -> default_header(Value).

-spec 'Content-Language'(sip_header_value()) -> term().
'Content-Language'(Value) -> default_header(Value).

-spec 'Content-Length'(sip_header_value()) -> integer().
'Content-Length'(Value) -> bstring:to_integer(Value).

-spec 'Content-Type'(sip_header_value()) -> term().
'Content-Type'(Value) -> default_header(Value).

-spec 'Cseq'(sip_header_value()) -> term().
'Cseq'(Value) -> default_header(Value).

-spec 'Date'(sip_header_value()) -> term().
'Date'(Value) -> default_header(Value).

-spec 'Error-Info'(sip_header_value()) -> term().
'Error-Info'(Value) -> default_header(Value).

-spec 'Expires'(sip_header_value()) -> term().
'Expires'(Value) -> default_header(Value).

-spec 'From'(sip_header_value()) -> term().
'From'(Value) -> default_header(Value).

-spec 'In-Reply-To'(sip_header_value()) -> term().
'In-Reply-To'(Value) -> default_header(Value).

-spec 'Max-Forwards'(sip_header_value()) -> term().
'Max-Forwards'(Value) -> default_header(Value).

-spec 'Min-Expires'(sip_header_value()) -> term().
'Min-Expires'(Value) -> default_header(Value).

-spec 'Mime-Version'(sip_header_value()) -> term().
'Mime-Version'(Value) -> default_header(Value).

-spec 'Organization'(sip_header_value()) -> term().
'Organization'(Value) -> default_header(Value).

-spec 'Priority'(sip_header_value()) -> term().
'Priority'(Value) -> default_header(Value).

-spec 'Proxy-Authenticate'(sip_header_value()) -> term().
'Proxy-Authenticate'(Value) -> default_header(Value).

-spec 'Proxy-Authorization'(sip_header_value()) -> term().
'Proxy-Authorization'(Value) -> default_header(Value).

-spec 'Proxy-Require'(sip_header_value()) -> term().
'Proxy-Require'(Value) -> default_header(Value).

-spec 'Record-Route'(sip_header_value()) -> term().
'Record-Route'(Value) -> default_header(Value).

-spec 'Reply-To'(sip_header_value()) -> term().
'Reply-To'(Value) -> default_header(Value).

-spec 'Require'(sip_header_value()) -> term().
'Require'(Value) -> default_header(Value).

-spec 'Retry-After'(sip_header_value()) -> term().
'Retry-After'(Value) -> default_header(Value).

-spec 'Route'(sip_header_value()) -> term().
'Route'(Value) -> default_header(Value).

-spec 'Server'(sip_header_value()) -> term().
'Server'(Value) -> default_header(Value).

-spec 'Subject'(sip_header_value()) -> term().
'Subject'(Value) -> default_header(Value).

-spec 'Supported'(sip_header_value()) -> term().
'Supported'(Value) -> default_header(Value).

-spec 'Timestamp'(sip_header_value()) -> term().
'Timestamp'(Value) -> default_header(Value).

-spec 'To'(sip_header_value()) -> term().
'To'(Value) -> default_header(Value).

-spec 'Unsupported'(sip_header_value()) -> term().
'Unsupported'(Value) -> default_header(Value).

-spec 'User-Agent'(sip_header_value()) -> term().
'User-Agent'(Value) -> default_header(Value).

-spec 'Via'(sip_header_value()) -> term().
'Via'(Value) -> default_header(Value).

-spec 'Warning'(sip_header_value()) -> term().
'Warning'(Value) -> default_header(Value).

-spec 'Www-Authenticate'(sip_header_value()) -> term().
'Www-Authenticate'(Value) -> default_header(Value).

-spec default_header(sip_header_value()) -> term().
default_header(HeaderValue) ->
    [Value | Params] = binary:split(HeaderValue, <<";">>, [global]),
    {Value, [parse_param(Param) || Param <- Params]}.
    

%% --------------------------------------------------------------------------
%% Internal Decode Functions
%% --------------------------------------------------------------------------
parse_param(Param) ->
    [Key, Value] = binary:split(Param, <<"=">>),
    {bstring:strip(Key), bstring:strip(Value)}.

parse_value(Type, Value, LnNo, Opaque, HeaderHooks) ->
    Fun = try
              dict:fetch(Type, HeaderHooks)
          catch error:badarg ->
                  dict:fetch(default, HeaderHooks)
          end,
    stq:header(Type, Fun(strip_tabcr(Value)), LnNo, Opaque).

strip_tabcr(Binary) ->
    bstring:strip(binary:replace(Binary,<<"\r\n\t">>, <<" ">>,[global])).

incr(undefined) ->
    undefined;
incr(Num) ->
    Num + 1.
