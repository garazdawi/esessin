%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(test_esessin_header).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

sip_header_decode_test_() ->
    [{binary_to_list(Header), ?_test(simple_decode(Exp, Header))} ||
        {Exp, Header} <- [{'Accept',<<"Accept">>},
                          {'Accept-Encoding',<<"Accept-Encoding">>},
                          {'Alert-Info',<<"Alert-Info">>},
                          {'Allow',<<"Allow">>},
                          {'Authentication-Info',<<"Authentication-Info">>},
                          {'Authorization',<<"Authorization">>},
                          {'Call-Id',<<"Call-ID">>},
                          {'Call-Info',<<"Call-Info">>},
                          {'Contact',<<"Contact">>},
                          {'Content-Disposition',<<"Content-Disposition">>},
                          {'Content-Encoding',<<"Content-Encoding">>},
                          {'Content-Language',<<"Content-Language">>},
                          {'Content-Type',<<"Content-Type">>},
                          {'Cseq',<<"CSeq">>},
                          {'Date',<<"Date">>},
                          {'Error-Info',<<"Error-Info">>},
                          {'Expires',<<"Expires">>},
                          {'From',<<"From">>},
                          {'In-Reply-To',<<"In-Reply-To">>},
                          {'Max-Forwards',<<"Max-Forwards">>},
                          {'Min-Expires',<<"Min-Expires">>},
                          {'Mime-Version',<<"MIME-Version">>},
                          {'Organization',<<"Organization">>},
                          {'Priority',<<"Priority">>},
                          {'Proxy-Authenticate',<<"Proxy-Authenticate">>},
                          {'Proxy-Authorization',<<"Proxy-Authorization">>},
                          {'Proxy-Require',<<"Proxy-Require">>},
                          {'Record-Route',<<"Record-Route">>},
                          {'Reply-To',<<"Reply-To">>},
                          {'Require',<<"Require">>},
                          {'Retry-After',<<"Retry-After">>},
                          {'Route',<<"Route">>},
                          {'Server',<<"Server">>},
                          {'Subject',<<"Subject">>},
                          {'Supported',<<"Supported">>},
                          {'Timestamp',<<"Timestamp">>},
                          {'To',<<"To">>},
                          {'Unsupported',<<"Unsupported">>},
                          {'User-Agent',<<"User-Agent">>},
                          {'Via',<<"Via">>},
                          {'Warning',<<"Warning">>},
                          {'Www-Authenticate',<<"WWW-Authenticate">>},
                          {<<"Test">>,<<"test">>}]].

sip_header_compact_decode_test_() ->
    [{binary_to_list(Header), ?_test(simple_decode(Exp, Header))} ||
        {Exp, Header} <- [{'Accept-Contact',<<"a">>},
			  {'Allow-Events',<<"u">>},
			  {'Call-ID',<<"i">>},
			  {'Contact',<<"m">>},
			  {'Content-Encoding',<<"e">>},
			  {'Content-Length',<<"l">>},
			  {'Content-Type',<<"c">>},
			  {'Event',<<"o">>},
			  {'From',<<"f">>},
			  {'Identity',<<"y">>},
			  {'Identity-Info',<<"n">>},
			  {'Refer-To',<<"r">>},
			  {'Referred-By',<<"b">>},
			  {'Reject-Contact',<<"j">>},
			  {'Request-Disposition',<<"d">>},
			  {'Session-Expires',<<"x">>},
			  {'Subject',<<"s">>},
			  {'Supported',<<"k">>},
			  {'To',<<"t">>},
			  {'Via',<<"v">>}]].

http_header_decode_test_() ->
    [{binary_to_list(Header), ?_test(simple_decode(Header, Header))} ||
        Header <- [<<"Cache-Control">>,
                   <<"Connection">>,
                   <<"Pragma">>,
                   <<"Upgrade">>,
                   <<"Accept-Charset">>,
                   <<"Accept-Language">>,
                   <<"Host">>,
                   <<"If-Modified-Since">>,
                   <<"If-Match">>,
                   <<"If-None-Match">>,
                   <<"If-Range">>,
                   <<"If-Unmodified-Since">>,
                   <<"Range">>,
                   <<"Referer">>,
                   <<"Age">>,
                   <<"Location">>,
                   <<"Public">>,
                   <<"Vary">>,
                   <<"Content-Base">>,
                   <<"Content-Location">>,
                   <<"Content-Md5">>,
                   <<"Content-Range">>,
                   <<"Etag">>,
                   <<"Last-Modified">>,
                   <<"Accept-Ranges">>,
                   <<"Set-Cookie">>,
                   <<"Set-Cookie2">>,
                   <<"X-Forwarded-For">>,
                   <<"Cookie">>,
                   <<"Keep-Alive">>,
                   <<"Proxy-Connection">>]].


simple_decode('Content-Length' = Exp, Header) ->
    {ok, O, _Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
                                    Header/binary, ": 0\r\n\r\n">>, []),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual([Exp], stq:header_fields(O)),
    ?assertEqual([{Exp,[{0,undefined}]}], stq:headers(O));
simple_decode(Exp, Header) ->
    {ok, O, _Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
                                    Header/binary, ": 0\r\n"
                                    "Content-Length: 0\r\n\r\n">>, []),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual([Exp,'Content-Length'], stq:header_fields(O)),
    ?assertEqual([{Exp,[{{<<"0">>,[]},undefined}]},
                  {'Content-Length',[{0,undefined}]}], stq:headers(O)).
                       

line_no_decode_test() ->
    {ok, O, _Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
                                    "Via: petter\r\n"
                                    "Content-Length: 0\r\n\r\n">>,
				   [{keep_line_info, true}]),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(['Via','Content-Length'], stq:header_fields(O)),
    ?assertEqual([{'Via',[{{<<"petter">>,[]},10}]},
                  {'Content-Length',[{0,20}]}], stq:headers(O)).

multi_value_test_() ->
    [{"Same line",fun same_line/0},
     {"Same line Auth", fun same_line_auth/0},
     {"Different line",fun different_line/0},
     {"Combined", fun combined/0},
     {"Combined w/o line info", fun combined_no_line_info/0}].

same_line() ->
    {ok, O, _Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
				     "Via: petter, adam\r\n"
				     "Content-Length: 0\r\n\r\n">>,
				    [{keep_line_info, true}]),
    
    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(['Via','Content-Length'], stq:header_fields(O)),
    ?assertEqual([{'Via',[{{<<"petter">>,[]},10},
			  {{<<"adam">>,[]},11}]},
                  {'Content-Length',[{0,20}]}], stq:headers(O)).

different_line() ->
    
    {ok, O, _Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
				     "Via: petter\r\n"
				     "Via: adam\r\n"
				     "Content-Length: 0\r\n\r\n">>,
				    [{keep_line_info, true}]),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(['Via','Content-Length'], stq:header_fields(O)),
    ?assertEqual([{'Via',[{{<<"petter">>,[]},10},
			  {{<<"adam">>,[]},20}]},
                  {'Content-Length',[{0,30}]}], stq:headers(O)).

combined() ->
    {ok, O, _Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
				     "Via: petter, adam\r\n"
				     "Via: lukas\r\n"
				     "Content-Length: 0\r\n\r\n">>,
				    [{keep_line_info, true}]),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(['Via','Content-Length'], stq:header_fields(O)),
    ?assertEqual([{'Via',[{{<<"petter">>,[]},10},
			  {{<<"adam">>,[]},11},
			  {{<<"lukas">>,[]},20}]},
                  {'Content-Length',[{0,30}]}], stq:headers(O)).

combined_no_line_info() ->
    {ok, O, _Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
				     "Via: petter, adam\r\n"
				     "Via: lukas\r\n"
				     "Content-Length: 0\r\n\r\n">>,
				    [{keep_line_info, false}]),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(['Via','Content-Length'], stq:header_fields(O)),
    ?assertEqual([{'Via',[{{<<"petter">>,[]},undefined},
			  {{<<"adam">>,[]},undefined},
			  {{<<"lukas">>,[]},undefined}]},
                  {'Content-Length',[{0,undefined}]}], stq:headers(O)).

same_line_auth() ->
    
    {ok, O, _Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
				     "WWW-Authenticate: petter, adam\r\n"
				     "Content-Length: 0\r\n\r\n">>,
				    [{keep_line_info, true},
				     {header_hooks, none}]),
    
    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(['Www-Authenticate','Content-Length'], stq:header_fields(O)),
    ?assertEqual([{'Www-Authenticate',[{<<"petter, adam">>,10}]},
                  {'Content-Length',[{0,20}]}], stq:headers(O)).

header_hook_test_() ->
    [{"Overwrite 'Via'", fun hh_overwrite/0}].

hh_overwrite() ->
    {ok, O, _Rest} =
	esessin:decode(
	  <<"INVITE sip:lukas@localhost SIP/1.0\r\n",
	   "Via: petter, adam\r\n"
	   "Via: lukas\r\n"
	   "Content-Length: 0\r\n\r\n">>,
	  [{keep_line_info, true},
	   {header_hooks,[{'Via', fun erlang:binary_to_list/1}]}]),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(['Via','Content-Length'], stq:header_fields(O)),
    ?assertEqual([{'Via',[{"petter",10},
			  {"adam",11},
			  {"lukas",20}]},
                  {'Content-Length',[{0,30}]}], stq:headers(O)).

