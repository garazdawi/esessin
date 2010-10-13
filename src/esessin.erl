%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

%% TODO: Figure out how to handle SIP compact form headers, see RFC 3261 §7.3.1
%% TODO: Figure out how to handle multi headers, see RFC 3261 §7.3.1
%%       (i.e. Route: a,b vs Route: a\r\nRoute:b )

-module(esessin).

-include("stq.hrl").

-export([compile_decode_options/1,
	 compile_encode_options/1,
	 decode/2,
	 encode/2]).


-type decode_options() :: [{on_parse_error, ignore | fail | function()}].
-type encode_options() :: [].

-record(decode_state, { state = method :: method | header | body,
			headers :: list(),
			buffer = <<>> :: binary(),
			on_parse_error = fail,
			stq :: stq_opaque() }).
-record(encode_opts, {}).

%% --------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------
%% @doc Compile all encoding options for faster lookups
-spec compile_decode_options(Opts :: decode_options()) ->
    Opaque :: term().
compile_decode_options(Opts) ->
    set_decode_opts(#decode_state{ }, Opts).

-spec compile_encode_options(Opts :: encode_options()) ->
    Opaque :: term().
compile_encode_options(Opts) ->
    set_encode_opts(#encode_opts{ }, Opts).

%% @doc Decode a SIP binary to a STQ data structure
-spec decode(binary(), term() | decode_options()) ->
    {more, Opaque :: term()} | {ok, Data :: stq_opaque(), Rest :: binary()}.
decode(Bin, Opts) when is_list(Opts) ->
    decode(Bin, compile_decode_options(Opts));
decode(Bin, #decode_state{ buffer = Buff } = State) when Buff =/= <<>> ->
    decode(<<Buff/binary,Bin/binary>>, State#decode_state{ buffer = <<>> });
decode(Bin, Opaque) ->
    parse(Bin, Opaque).

%% @doc Encode a STQ data structure to a SIP binary
-spec encode(Data :: stq_opaque(), Opts :: proplist()) ->
    binary().
encode(Data, Opts) when is_list(Opts) ->
    encode(Data, compile_encode_options(Opts));
encode(Data, _Opts) ->
    Data.

%% --------------------------------------------------------------------------
%% Internal Decode Functions
%% --------------------------------------------------------------------------

parse(Bin, #decode_state{ state = method } = State) ->
    case esi_parser:decode_packet(sip_bin, Bin, []) of
        {ok, {sip_request, Method, Uri, Vsn}, Rest} ->
            parse(Rest, State#decode_state{ state = header,
                                     headers = [],
                                     stq = stq:new(Method, Uri, Vsn) });
        {ok, {sip_response, Vsn, Code, Msg}, Rest} ->
            parse(Rest, State#decode_state{ state = header,
                                     headers = [],
                                     stq = stq:new(Code, Msg, Vsn) });
        {ok, {sip_error, Line}, Rest} ->
	    parse_error(Line, Rest, Bin, State);
        {more, _HowMuch} ->
            {more, State#decode_state{ buffer = Bin }};
        {error, Reason} ->
            erlang:error(Reason, [Bin, State])
    end;
parse(Bin, #decode_state{ state = header, headers = Headers } = State) ->
    case esi_parser:decode_packet(siph_bin, Bin, []) of
        {ok, {sip_header, _, Field, _, Value}, Rest} ->
            parse(Rest, State#decode_state{
			  headers = [{Field, Value} | Headers]});
        {ok, {sip_error, Line}, Rest} ->
	    parse_error(Line, Rest, Bin, State);
        {ok, sip_eoh, Body} ->
            parse(Body, State#decode_state{
                          state = body,
                          stq = stq:headers(lists:reverse(Headers),
                                            State#decode_state.stq) });
        {more, _HowMuch} ->
            {more, State#decode_state{ buffer = Bin } };
        {error, Reason} ->
            erlang:error(Reason, [Bin, State])
    end;
parse(Msg, #decode_state{ state = body, stq = Stq } = State) ->
    ContentLength = hd(stq:header('Content-Length', Stq)),
    Length = bstring:to_integer(ContentLength),
    case Msg of
	<<Body:Length/binary, Rest/binary>> ->
	    {ok, stq:body(Body, State#decode_state.stq), Rest};
	Msg ->
	    {more, State#decode_state{ buffer = Msg } }
    end.

parse_error(Line, Rest, _Bin, #decode_state{ state = method,
					     on_parse_error = fail } = State)
  when Line =:= <<"\r\n">>; Line =:= <<"\n">> ->
    parse(Rest, State);
parse_error(Line, Rest, Bin, #decode_state{ on_parse_error = fail } = State) ->
    erlang:error({parse_failed, Line, Rest},[Line, Rest, Bin, State]);
parse_error(_, Rest, _Bin, #decode_state{ on_parse_error = ignore } = State) ->
    parse(Rest, State);
parse_error(Line, Rest, _Bin, #decode_state{ on_parse_error = Fun } = State)
  when is_function(Fun) ->
    NewRest = Fun(Line, Rest),
    parse(NewRest, State).

set_decode_opts(State, [{on_parse_error, Action} | Rest]) ->
    set_decode_opts(State#decode_state{ on_parse_error = Action }, Rest);
set_decode_opts(State, [_ | _] = Rest) ->
    erlang:error(badarg, [State, Rest]);
set_decode_opts(State, []) ->
    State.

%% --------------------------------------------------------------------------
%% Internal Encode Functions
%% --------------------------------------------------------------------------

set_encode_opts(State, [_ | _] = Rest) ->
    erlang:error(badarg, [State, Rest]);
set_encode_opts(State, []) ->
    State.
