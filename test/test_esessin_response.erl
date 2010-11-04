%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(test_esessin_response).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    response_decode().

decode_error_test_() ->
    [{Label, ?_test(parse_error(Value))}
     || {Label, Value} <- [{"not_sip",<<"HTTP/1.1 200 OK">>},
                           {"char version",<<"SIP/K.0 200 OK">>},
                           {"three version",<<"SIP/2.0.1 200 OK">>},
                           {"two parts",<<"SIP/2.0 200">>}
                          ]].

response_decode() ->
    [{binary_to_list(Response), ?_test(simple_decode(Exp, Response))} ||
        {Exp, {Response}} <- [
                            {{100,<<"Trying">>},
                             {<<"100 Trying">>}},
                            {{180,<<"Ringing">>},
                             {<<"180 Ringing">>}},
                            {{181,<<"Call Is Being Forwarded">>},
                             {<<"181 Call Is Being Forwarded">>}},
                            {{182,<<"Queued">>},
                             {<<"182 Queued">>}},
                            {{183,<<"Session Progress">>},
                             {<<"183 Session Progress">>}},
                            {{200,<<"OK">>},
                             {<<"200 OK">>}},
                            {{202,<<"Accepted">>},
                             {<<"202 Accepted">>}},
                            {{300,<<"Multiple Choices">>},
                             {<<"300 Multiple Choices">>}},
                            {{301,<<"Moved Permanently">>},
                             {<<"301 Moved Permanently">>}},
                            {{302,<<"Moved Temporarily">>},
                             {<<"302 Moved Temporarily">>}},
                            {{305,<<"Use Proxy">>},
                             {<<"305 Use Proxy">>}},
                            {{380,<<"Alternative Service">>},
                             {<<"380 Alternative Service">>}},
                            {{400,<<"Bad Request">>},
                             {<<"400 Bad Request">>}},
                            {{401,<<"Unauthorized">>},
                             {<<"401 Unauthorized">>}},
                            {{402,<<"Payment Required">>},
                             {<<"402 Payment Required">>}},
                            {{403,<<"Forbidden">>},
                             {<<"403 Forbidden">>}},
                            {{404,<<"Not Found">>},
                             {<<"404 Not Found">>}},
                            {{405,<<"Method Not Allowed">>},
                             {<<"405 Method Not Allowed">>}},
                            {{406,<<"Not Acceptable">>},
                             {<<"406 Not Acceptable">>}},
                            {{407,<<"Proxy Authentication Required">>},
                             {<<"407 Proxy Authentication Required">>}},
                            {{408,<<"Request Timeout">>},
                             {<<"408 Request Timeout">>}},
                            {{409,<<"Conflict">>},
                             {<<"409 Conflict">>}},
                            {{410,<<"Gone">>},
                             {<<"410 Gone">>}},
                            {{412,<<"Conditional Request Failed">>},
                             {<<"412 Conditional Request Failed">>}},
                            {{413,<<"Request Entity Too Large">>},
                             {<<"413 Request Entity Too Large">>}},
                            {{414,<<"Request-URI Too Long">>},
                             {<<"414 Request-URI Too Long">>}},
                            {{415,<<"Unsupported Media Type">>},
                             {<<"415 Unsupported Media Type">>}},
                            {{416,<<"Unsupported URI Scheme">>},
                             {<<"416 Unsupported URI Scheme">>}},
                            {{417,<<"Unknown Resource-Priority">>},
                             {<<"417 Unknown Resource-Priority">>}},
                            {{420,<<"Bad Extension">>},
                             {<<"420 Bad Extension">>}},
                            {{421,<<"Extension Required">>},
                             {<<"421 Extension Required">>}},
                            {{422,<<"Session Interval Too Small">>},
                             {<<"422 Session Interval Too Small">>}},
                            {{423,<<"Interval Too Brief">>},
                             {<<"423 Interval Too Brief">>}},
                            {{424,<<"Bad Location Information">>},
                             {<<"424 Bad Location Information">>}},
                            {{428,<<"Use Identity Header">>},
                             {<<"428 Use Identity Header">>}},
                            {{429,<<"Provide Referrer Identity">>},
                             {<<"429 Provide Referrer Identity">>}},
                            {{433,<<"Anonymity Disallowed">>},
                             {<<"433 Anonymity Disallowed">>}},
                            {{436,<<"Bad Identity-Info">>},
                             {<<"436 Bad Identity-Info">>}},
                            {{437,<<"Unsupported Certificate">>},
                             {<<"437 Unsupported Certificate">>}},
                            {{438,<<"Invalid Identity Header">>},
                             {<<"438 Invalid Identity Header">>}},
                            {{480,<<"Temporarily Unavailable">>},
                             {<<"480 Temporarily Unavailable">>}},
                            {{481,<<"Call/Transaction Does Not Exist">>},
                             {<<"481 Call/Transaction Does Not Exist">>}},
                            {{482,<<"Loop Detected">>},
                             {<<"482 Loop Detected">>}},
                            {{483,<<"Too Many Hops">>},
                             {<<"483 Too Many Hops">>}},
                            {{484,<<"Address Incomplete">>},
                             {<<"484 Address Incomplete">>}},
                            {{485,<<"Ambiguous">>},
                             {<<"485 Ambiguous">>}},
                            {{486,<<"Busy Here">>},
                             {<<"486 Busy Here">>}},
                            {{487,<<"Request Terminated">>},
                             {<<"487 Request Terminated">>}},
                            {{488,<<"Not Acceptable Here">>},
                             {<<"488 Not Acceptable Here">>}},
                            {{489,<<"Bad Event">>},
                             {<<"489 Bad Event">>}},
                            {{491,<<"Request Pending">>},
                             {<<"491 Request Pending">>}},
                            {{493,<<"Undecipherable">>},
                             {<<"493 Undecipherable">>}},
                            {{494,<<"Security Agreement Required">>},
                             {<<"494 Security Agreement Required">>}},
                            {{500,<<"Server Internal Error">>},
                             {<<"500 Server Internal Error">>}},
                            {{501,<<"Not Implemented">>},
                             {<<"501 Not Implemented">>}},
                            {{502,<<"Bad Gateway">>},
                             {<<"502 Bad Gateway">>}},
                            {{503,<<"Service Unavailable">>},
                             {<<"503 Service Unavailable">>}},
                            {{504,<<"Server Time-out">>},
                             {<<"504 Server Time-out">>}},
                            {{505,<<"Version Not Supported">>},
                             {<<"505 Version Not Supported">>}},
                            {{513,<<"Message Too Large">>},
                             {<<"513 Message Too Large">>}},
                            {{580,<<"Precondition Failure">>},
                             {<<"580 Precondition Failure">>}},
                            {{600,<<"Busy Everywhere">>},
                             {<<"600 Busy Everywhere">>}},
                            {{603,<<"Decline">>},
                             {<<"603 Decline">>}},
                            {{604,<<"Does Not Exist Anywhere">>},
                             {<<"604 Does Not Exist Anywhere">>}},
                            {{606,<<"Not Acceptable">>},
                             {<<"606 Not Acceptable">>}}
                           ]].

simple_decode({Code, Msg}, Response) ->
    {ok, O, Rest} = esessin:decode(<<"SIP/2.0 ",Response/binary,"\r\n"
                                    "Content-Type: application/sdp\r\n"
                                    "Content-Length: 0\r\n\r\n">>, []),

    io:format("Opaque = ~p~n",[O]),

    ?assertEqual(Code, stq:code(O)),
    ?assertEqual(bstring:to_lower(Msg), stq:resp_msg(O)),
    ?assertEqual({2,0}, stq:vsn(O)),
    ?assertEqual(<<>>, stq:body(O)),
    ?assertEqual(<<>>, Rest).

parse_error(Method) ->

    Str = <<Method/binary, "\r\n"
	   "Via: home.se\r\n"
	   "Content-Length: 0\r\n\r\n">>,
    
    ?assertError(
       {parse_failed, _, _},esessin:decode(Str, [])),
    ?assertMatch(
       {more, _}, esessin:decode(Str ,[{on_parse_error, ignore}])),	

    Self = self(),
    Fun = fun(Line, Rest) ->
		  Self ! Line,
		  Rest
	  end,
    
    ?assertMatch(
       {more, _}, esessin:decode(Str, [{on_parse_error, Fun}])),

    ?assertEqual(Str, receive_all(<<>>)).

receive_all(Acc) ->
    receive
	Bin ->
	    receive_all(<<Acc/binary,Bin/binary>>)
    after 1 ->
	    io:format(Acc),
	    Acc
    end.
	    
