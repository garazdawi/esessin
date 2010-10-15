%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(esi_parser).

-export([decode_packet/3,
	 decode_method/2,
	 decode_header/2]).


-type proplist() :: list({atom(), term()}).
-type sip_uri() :: binary().
-type vsn() :: {integer(), integer()}.
-type sip_request() :: {sip_request, Method :: atom() | binary(),
			Uri :: sip_uri(),
			Vsn :: vsn() }.
-type sip_response() :: {sip_response, Vsn :: vsn(), Code :: integer(),
			 Msg :: binary()}.
-type sip_parse_header() :: {sip_header, term(), Field :: atom() | binary(),
		       term(), Value :: binary()}.
-type sip_error() :: {sip_error, Line :: binary()}.

%% --------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------
%% @doc Decode a SIP packet line
-spec decode_packet(sip_bin , binary(), Opts :: proplist()) ->
    {more, undefined | integer()} |
    {ok, sip_request() | sip_response() | sip_error(), Rest :: binary()};
                   (siph_bin, binary(), Opts :: proplist()) ->
    {more, undefined | integer()} |
    {ok, sip_parse_header() | sip_eoh | sip_error(), Rest :: binary()}.
decode_packet(sip_bin, Bin, Opts) ->
    decode_method(Bin, Opts);
decode_packet(siph_bin, Bin, Opts) ->
    decode_header(Bin, Opts).

%% @doc Decode a SIP Request/Response line
-spec decode_method(binary(), Opts :: proplist()) ->
    {more, undefined | integer()} |
    {ok, sip_request() | sip_response() | sip_error(), Rest :: binary()}.
decode_method(Bin, Opts) ->
    case binary:split(Bin, [<<"\n">>]) of
        [Line, Rest] ->
            try
                {ok, parse_line(bstring:to_lower(Line), Opts), Rest}
            catch throw:could_not_parse ->
                    {ok, {sip_error, <<Line/binary, "\n">>}, Rest}
            end;
        _ ->
            {more, undefined}
    end.
    
%% @doc Decode a SIP header line
-spec decode_header(binary(), Opts :: proplist()) ->
    {more, undefined | integer()} |
    {ok, sip_parse_header() | sip_eoh | sip_error(), Rest :: binary()}.
decode_header(Data, _Opts) ->
    case erlang:decode_packet(httph_bin, Data, []) of
        {ok, {http_header, Code, Field, Unused, Value}, Rest} ->
            % TODO: Strip \t(\r)\n sequences, see RFC 3261 § 7.3.1
            {ok, {sip_header, Code, parse_header_field(Field), Unused, Value}, Rest};
        {ok, http_eoh, Rest} ->
            {ok, sip_eoh, Rest};
        {ok, {http_error, Line}, Rest} ->
            {ok, {sip_error, Line}, Rest};
        Else ->
            Else
    end.

%% --------------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------------
parse_line(<<>>, _Opts) ->
    throw(could_not_parse);
parse_line(Line, _Opts) ->
    case binary:split(Line, [<<" ">>,<<"\r">>],[global,trim]) of
        [<<"sip",_/binary>> = Vsn, Code, Msg] ->
            {sip_response, parse_vsn(Vsn), parse_code(Code), Msg};
        [Method, Uri, Vsn] ->
            {sip_request, parse_method(Method), parse_uri(Uri), parse_vsn(Vsn)};
        _Else ->
            throw(could_not_parse)
    end.

%% Method line parse functions
parse_method(<<"invite">>) ->
    invite;
parse_method(<<"ack">>) ->
    ack;
parse_method(<<"bye">>) ->
    bye;
parse_method(<<"cancel">>) ->
    cancel;
parse_method(<<"option">>) ->
    option;
parse_method(<<"register">>) ->
    register;
parse_method(Method) ->
    Method.


parse_vsn(<<"sip/", Vsn/binary>>) ->
    case binary:split(Vsn, <<".">>,[global]) of
        [Major, Minor] ->
            try
                {list_to_integer(binary_to_list(Major)),
                 list_to_integer(binary_to_list(Minor))}
            catch error:badarg ->
                    throw(could_not_parse)
            end;
        _Else ->
            throw(could_not_parse)
    end;
parse_vsn(_) ->
    throw(could_not_parse).

parse_code(Code) ->
    try
        list_to_integer(binary_to_list(Code))
    catch error:badarg ->
            throw(could_not_parse)
    end.

parse_uri(Uri) ->
    Uri.


%% sip header_field parse functions
-spec parse_header_field(binary() | atom()) ->
    atom() | binary().
parse_header_field(<<"Alert-Info">>) -> 'Alert-Info';
parse_header_field(<<"Authentication-Info">>) -> 'Authentication-Info';
parse_header_field(<<"Call-Id">>) -> 'Call-Id';
parse_header_field(<<"Call-Info">>) -> 'Call-Info';
parse_header_field(<<"Contact">>) -> 'Contact';
parse_header_field(<<"Content-Disposition">>) -> 'Content-Disposition';
parse_header_field(<<"Cseq">>) -> 'Cseq';
parse_header_field(<<"Error-Info">>) -> 'Error-Info';
parse_header_field(<<"In-Reply-To">>) -> 'In-Reply-To';
parse_header_field(<<"Min-Expires">>) -> 'Min-Expires';
parse_header_field(<<"Mime-Version">>) -> 'Mime-Version';
parse_header_field(<<"Organization">>) -> 'Organization';
parse_header_field(<<"Priority">>) -> 'Priority';
parse_header_field(<<"Proxy-Require">>) -> 'Proxy-Require';
parse_header_field(<<"Record-Route">>) -> 'Record-Route';
parse_header_field(<<"Reply-To">>) -> 'Reply-To';
parse_header_field(<<"Require">>) -> 'Require';
parse_header_field(<<"Route">>) -> 'Route';
parse_header_field(<<"Subject">>) -> 'Subject';
parse_header_field(<<"Supported">>) -> 'Supported';
parse_header_field(<<"Timestamp">>) -> 'Timestamp';
parse_header_field(<<"To">>) -> 'To';
parse_header_field(<<"Unsupported">>) -> 'Unsupported';
%% Compact header parse functions
%% see http://www.iana.org/assignments/sip-parameters
parse_header_field(<<"A">>) -> 'Accept-Contact';
parse_header_field(<<"U">>) -> 'Allow-Events';
parse_header_field(<<"I">>) -> 'Call-ID';
parse_header_field(<<"M">>) -> 'Contact';
parse_header_field(<<"E">>) -> 'Content-Encoding';
parse_header_field(<<"L">>) -> 'Content-Length';
parse_header_field(<<"C">>) -> 'Content-Type';
parse_header_field(<<"O">>) -> 'Event';
parse_header_field(<<"F">>) -> 'From';
parse_header_field(<<"Y">>) -> 'Identity';
parse_header_field(<<"N">>) -> 'Identity-Info';
parse_header_field(<<"R">>) -> 'Refer-To';
parse_header_field(<<"B">>) -> 'Referred-By';
parse_header_field(<<"J">>) -> 'Reject-Contact';
parse_header_field(<<"D">>) -> 'Request-Disposition';
parse_header_field(<<"X">>) -> 'Session-Expires';
parse_header_field(<<"S">>) -> 'Subject';
parse_header_field(<<"K">>) -> 'Supported';
parse_header_field(<<"T">>) -> 'To';
parse_header_field(<<"V">>) -> 'Via';
%% http header_field reversal
parse_header_field('Cache-Control') -> <<"Cache-Control">>;
parse_header_field('Connection') -> <<"Connection">>;
parse_header_field('Pragma') -> <<"Pragma">>;
parse_header_field('Upgrade') -> <<"Upgrade">>;
parse_header_field('Accept-Charset') -> <<"Accept-Charset">>;
parse_header_field('Accept-Language') -> <<"Accept-Language">>;
parse_header_field('Host') -> <<"Host">>;
parse_header_field('If-Modified-Since') -> <<"If-Modified-Since">>;
parse_header_field('If-Match') -> <<"If-Match">>;
parse_header_field('If-None-Match') -> <<"If-None-Match">>;
parse_header_field('If-Range') -> <<"If-Range">>;
parse_header_field('If-Unmodified-Since') -> <<"If-Unmodified-Since">>;
parse_header_field('Range') -> <<"Range">>;
parse_header_field('Referer') -> <<"Referer">>;
parse_header_field('Age') -> <<"Age">>;
parse_header_field('Location') -> <<"Location">>;
parse_header_field('Public') -> <<"Public">>;
parse_header_field('Vary') -> <<"Vary">>;
parse_header_field('Content-Base') -> <<"Content-Base">>;
parse_header_field('Content-Location') -> <<"Content-Location">>;
parse_header_field('Content-Md5') -> <<"Content-Md5">>;
parse_header_field('Content-Range') -> <<"Content-Range">>;
parse_header_field('Etag') -> <<"Etag">>;
parse_header_field('Last-Modified') -> <<"Last-Modified">>;
parse_header_field('Accept-Ranges') -> <<"Accept-Ranges">>;
parse_header_field('Set-Cookie') -> <<"Set-Cookie">>;
parse_header_field('Set-Cookie2') -> <<"Set-Cookie2">>;
parse_header_field('X-Forwarded-For') -> <<"X-Forwarded-For">>;
parse_header_field('Cookie') -> <<"Cookie">>;
parse_header_field('Keep-Alive') -> <<"Keep-Alive">>;
parse_header_field('Proxy-Connection') -> <<"Proxy-Connection">>;
parse_header_field(Else) -> Else.
