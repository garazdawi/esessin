-module(test_sip_transcoder).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    [fun ?MODULE:simple_invite/0,
     fun ?MODULE:more/0,
     fun ?MODULE:two_in_one/0].

simple_invite() ->
    {ok, O, <<>>} = sip_transcoder:decode(
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
		       "Test: 1\n\n">>, undefined, []),
    io:format("Opaque = ~p~n",[O]),
    ?assertEqual(invite,stq:method(O)),
    ?assertEqual({sip, <<"bob@biloxi.com">>},stq:uri(O)),
    ?assertEqual(<<"SIP/2.0">>, stq:vsn(O)),
    ?assertEqual(['Via','Max-Forwards', <<"To">>, 'From', <<"Call-Id">>,
		  <<"Cseq">>,<<"Contact">>,'Content-Type','Content-Length'],
		 stq:header_fields(O)),
    ?assertEqual(<<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>,stq:header(O,'Via')),
    ?assertEqual(<<"70">>, stq:header(O, 'Max-Forwards')),
    ?assertEqual(<<"Bob <sip:bob@biloxi.com>">>, stq:header(O, <<"To">>)),
    ?assertEqual(<<"Alice <sip:alice@atlanta.com>;tag=1928301774">>, stq:header(O, 'From')),
    ?assertEqual(<<"a84b4c76e66710@pc33.atlanta.com">>, stq:header(O, <<"Call-Id">>)),
    ?assertEqual(<<"314159 INVITE">>, stq:header(O, <<"Cseq">>)),
    ?assertEqual(<<"<sip:alice@pc33.atlanta.com>">>, stq:header(O, <<"Contact">>)),
    ?assertEqual(<<"application/sdp">>, stq:header(O, 'Content-Type')),
    ?assertEqual(<<"7">>, stq:header(O, 'Content-Length')),

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

    ?assertEqual(<<"Test: 1\n\n">>, stq:sdp(O)).

more() ->
    ok.

two_in_one() ->
    ok.
