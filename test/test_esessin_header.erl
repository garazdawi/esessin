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


simple_decode(Exp, Header) ->
    {ok, O, Rest} = esessin:decode(<<"INVITE sip:lukas@localhost SIP/1.0\r\n",
                                    Header/binary, ": test\r\n"
                                    "Content-Length: 0\r\n\r\n">>, []),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(invite, stq:method(O)),
    ?assertEqual(<<"sip:lukas@localhost">>, stq:uri(O)),
    ?assertEqual({1,0}, stq:vsn(O)),
    ?assertEqual([Exp,'Content-Length'], stq:header_fields(O)),
    ?assertEqual([{Exp,<<"test">>},
                  {'Content-Length',<<"0">>}], stq:headers(O)),
    ?assertEqual(<<>>, stq:body(O)),
    ?assertEqual(<<>>, Rest).
                       

