-module(test_essesin).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
     [fun ?MODULE:simple_invite/0,
      fun ?MODULE:more/0,
      fun ?MODULE:two_in_one/0,
      fun ?MODULE:complex_invite/0].

simple_invite() ->
    {ok, O, Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n"
                                    "Content-Type: application/sdp\r\n"
                                    "Content-Length: 0\r\n\r\n">>, []),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(invite, stq:method(O)),
    ?assertEqual(<<"sip:lukas@localhost">>, stq:uri(O)),
    ?assertEqual({1,0}, stq:vsn(O)),
    ?assertEqual(['Content-Type','Content-Length'], stq:header_fields(O)),
    ?assertEqual([{'Content-Type',<<"application/sdp">>},
                  {'Content-Length',<<"0">>}], stq:headers(O)),
    ?assertEqual(<<>>, Rest).
    


more() ->
    ok.

two_in_one() ->
    ok.


complex_invite() ->
    {ok, O, Rest} = esessin:decode(
		      <<"INVITE sip:bob@biloxi.com SIP/2.0\n"
		       "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds\n"
		       "Max-Forwards: 70\n"
		       "To: Bob <sip:bob@biloxi.com>\n"
		       "From: Alice <sip:alice@atlanta.com>;tag=1928301774\n"
		       "Call-ID: a84b4c76e66710@pc33.atlanta.com\n"
		       "CSeq: 314159 INVITE\n"
		       "Contact: <sip:alice@pc33.atlanta.com>\n"
		       "Content-Type: application/sdp\n"
		       "Content-Length: 7\n\n"
		       "Test: 1\n\n">>, []),
    io:format("Opaque = ~p~n",[O]),
    ?assertEqual(invite,stq:method(O)),
    ?assertEqual(<<"sip:bob@biloxi.com">>,stq:uri(O)),
    ?assertEqual({2,0}, stq:vsn(O)),
    ?assertEqual(['Via','Max-Forwards', <<"To">>, 'From', <<"Call-Id">>,
		  <<"Cseq">>,<<"Contact">>,'Content-Type','Content-Length'],
		 stq:header_fields(O)),
    ?assertEqual(<<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>,stq:header('Via',O)),
    ?assertEqual(<<"70">>, stq:header('Max-Forwards',O)),
    ?assertEqual(<<"Bob <sip:bob@biloxi.com>">>, stq:header(<<"To">>,O)),
    ?assertEqual(<<"Alice <sip:alice@atlanta.com>;tag=1928301774">>, stq:header('From',O)),
    ?assertEqual(<<"a84b4c76e66710@pc33.atlanta.com">>, stq:header(<<"Call-Id">>,O)),
    ?assertEqual(<<"314159 INVITE">>, stq:header(<<"Cseq">>, O)),
    ?assertEqual(<<"<sip:alice@pc33.atlanta.com>">>, stq:header(<<"Contact">>, O)),
    ?assertEqual(<<"application/sdp">>, stq:header('Content-Type', O)),
    ?assertEqual(<<"7">>, stq:header('Content-Length', O)),

    ?assertEqual([{'Via',<<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>},
		  {'Max-Forwards',<<"70">>},
		  {<<"To">>,<<"Bob <sip:bob@biloxi.com>">>},
		  {'From',<<"Alice <sip:alice@atlanta.com>;tag=1928301774">>},
		  {<<"Call-Id">>,<<"a84b4c76e66710@pc33.atlanta.com">>},
		  {<<"Cseq">>,<<"314159 INVITE">>},
		  {<<"Contact">>,<<"<sip:alice@pc33.atlanta.com>">>},
		  {'Content-Type',<<"application/sdp">>},
		  {'Content-Length',<<"7">>}],
		 stq:headers(O)),

    ?assertEqual(<<"Test: 1">>, stq:body(O)),
    ?assertEqual(<<"\n\n">>, Rest).
