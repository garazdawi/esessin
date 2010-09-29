%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(esessin).

-include("stq.hrl").

-export([decode/2, encode/2]).

-record(state, { state :: atom(),
		 headers :: list(),
		 buffer = <<>> :: binary(),
		 body_parser = esi_body_binary,
		 stq :: stq_opaque() }).

%% --------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------
%% @doc Decode a SIP binary to a STQ data structure
-spec decode(binary(), term() | proplist()) ->
    {more, Opaque :: term()} | {ok, Data :: stq_opaque(), Rest :: binary()}.
decode(Bin, Opts) when is_list(Opts) ->
    decode(Bin, set_opts(#state{ }, Opts));
decode(Bin, #state{ buffer = Buff } = State) when Buff =/= <<>> ->
    decode(<<Buff/binary,Bin/binary>>, State#state{ buffer = <<>> });
decode(Bin, Opaque) ->
    parse(Bin, Opaque).

%% @doc Encode a STQ data structure to a SIP binary
-spec encode(Data :: stq_opaque(), Opts :: proplist()) ->
    binary().
encode(Data, _Opts) ->
    Data.

%% --------------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------------

parse(Bin, #state{ state = undefined } = State) ->
    case esi_parser:parse_packet(Bin, []) of
	{ok, {sip_request, Method, Uri, Vsn}, Rest} ->
	    parse(Rest, State#state{ state = header,
				     headers = [],
				     stq = stq:new(Method, Uri, Vsn) });
	{ok, {sip_response, Vsn, Code, Msg}, Rest} ->
	    parse(Rest, State#state{ state = header,
				     headers = [],
				     stq = stq:new(Code, Msg, Vsn) });
	{ok, {sip_error, _Line}, Rest} ->
	    parse(Rest, State);
	{more, _HowMuch} ->
	    {more, State#state{ buffer = Bin }};
	{error, Reason} ->
	    error(Reason, [Bin, State])
    end;
parse(Bin, #state{ state = header, headers = Headers } = State) ->
    case esi_parser:parse_header(Bin, []) of
	{ok, {sip_header, _, Field, _, Value}, Rest} ->
	    parse(Rest, State#state{ headers = [{Field, Value} | Headers]});
	{ok, {sip_error, _Line}, Rest} ->
	    parse(Rest, State);
	{ok, sip_eoh, Body} ->
	    parse(Body, State#state{
			  state = body,
			  stq = stq:headers(lists:reverse(Headers),
					    State#state.stq) });
	{more, _HowMuch} ->
	    {more, State#state{ buffer = Bin } };
	{error, Reason} ->
	    error(Reason, [Bin, State])
    end;
parse(Msg, #state{ state = body, stq = Stq } = State) ->
    case list_to_integer(binary_to_list(stq:header('Content-Length', Stq))) of
	Length ->
	    case Msg of
		<<Body:Length/binary, Rest/binary>> ->
		    {ok, stq:body(Body, State#state.stq), Rest};
		Msg ->
		    {more, State#state{ buffer = Msg } }
	    end
    end.

set_opts(State, [{body_parser, Parser} | Rest]) ->
    set_opts(State#state{ body_parser = Parser }, Rest);
set_opts(State, []) ->
    State.
