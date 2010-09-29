%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(esi_parser).

-export([parse_packet/2, parse_header/2]).


-type proplist() :: list({atom(), term()}).
-type sip_uri() :: binary().
-type vsn() :: {integer(), integer()}.
-type sip_request() :: {sip_request, Method :: atom() | binary(),
			Uri :: sip_uri(),
			Vsn :: vsn() }.
-type sip_response() :: {sip_response, Vsn :: vsn(), Code :: integer(),
			 Msg :: binary()}.
-type sip_header() :: {sip_header, term(), Field :: atom() | binary(),
		       term(), Value :: binary()}.
-type sip_error() :: {sip_error, Line :: binary()}.

%% --------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------
%% @doc Parse a SIP Request/Response line
-spec parse_packet(binary(), Opts :: proplist()) ->
    {more, undefined | integer()} |
    {ok, sip_request() | sip_response() | sip_error(), Rest :: binary()} |
    {error, Reason :: term()}.
parse_packet(Bin, Opts) ->
    case binary:split(Bin, <<"\n">>) of
	[Line, Rest] ->
	    try
		{ok, parse_line(Line, Opts), Rest}
	    catch throw:could_not_parse ->
		    {ok, {sip_error, <<Line/binary, "\n">>}, Rest}
	    end;
	_ ->
	    {more, undefined}
    end.
    
%% @doc Parse a SIP header line
-spec parse_header(binary(), Opts :: proplist()) ->
    {more, undefined | integer()} |
    {ok, sip_header() | sip_eoh | sip_error(), Rest :: binary()} |
    {error, Reason :: term()}.
parse_header(Data, _Opts) ->
    case erlang:decode_packet(httph_bin, Data, []) of
	{ok, {http_header, Code, Field, Unused, Value}, Rest} ->
	    % TODO: Strip \t(\r)\n sequences, see RFC 3261 § 7.3.1
	    {ok, {sip_header, Code, Field, Unused, Value}, Rest};
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
parse_line(Line, _Opts) ->
    case binary:split(Line, <<" ">>,[global]) of
	[<<"SIP",_/binary>> = Vsn, Code, Msg] ->
	    {sip_response, parse_vsn(Vsn), parse_code(Code), Msg};
	[Method, Uri, Vsn] ->
	    {sip_request, parse_method(Method), parse_uri(Uri), parse_vsn(Vsn)};
	_Else ->
	    throw(could_not_parse)
    end.

parse_method(<<"INVITE">>) ->
    invite;
parse_method(<<"ACK">>) ->
    ack;
parse_method(<<"BYE">>) ->
    bye;
parse_method(<<"CANCEL">>) ->
    cancel;
parse_method(<<"OPTION">>) ->
    option;
parse_method(<<"REGISTER">>) ->
    register;
parse_method(Method) ->
    Method.


parse_vsn(<<"SIP/", Vsn/binary>>) ->
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
    end.

parse_code(Code) ->
    try
	list_to_integer(binary_to_list(Code))
    catch error:badarg ->
	    throw(could_not_parse)
    end.

parse_uri(Uri) ->
    Uri.
