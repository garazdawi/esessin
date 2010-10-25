%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(bstring).

%% API
-export([to_lower/1, to_upper/1, to_integer/1, strip/1, strip/2, strip/3]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec to_lower(binary()) -> binary().
to_lower(Binary) ->
    to_lower(Binary, <<>>).

to_lower(<<C,Rest/binary>>,Acc) when C =< $Z, C >= $A ->
    to_lower(Rest, <<Acc/binary, (C + $a - $A)>>);
to_lower(<<C, Rest/binary>>, Acc) ->
    to_lower(Rest, <<Acc/binary, C>>);
to_lower(<<>>, Acc) ->
    Acc.

-spec to_upper(binary()) -> binary().
to_upper(Binary) ->
    to_upper(Binary, <<>>).

to_upper(<<C,Rest/binary>>,Acc) when C =< $z, C >= $a ->
    to_upper(Rest, <<Acc/binary, (C + $A - $a)>>);
to_upper(<<C, Rest/binary>>, Acc) ->
    to_upper(Rest, <<Acc/binary, C>>);
to_upper(<<>>, Acc) ->
    Acc.

-spec to_integer(binary()) -> integer().
to_integer(Bin) ->
    list_to_integer(binary_to_list(Bin)).

%% @doc See string:strip/1,2,3
-spec strip(binary()) -> binary().
strip(Binary) ->
    strip(Binary, both).
strip(Binary, Dir) ->
    strip(Binary, Dir, $ ).
strip(Binary, left, Char) ->
    strip_int(Binary, [leading], Char, {<<>> , <<>>} );
strip(Binary, right, Char) ->
    strip_int(Binary, [trailing], Char, {<<>> , <<>>} );
strip(Binary, both, Char) ->
    strip_int(Binary, [leading, trailing], Char, {<<>> , <<>>}).

strip_int(<<Char,Rest/binary>>, [leading|_Rest] = Operations, Char, Acc) ->
    strip_int(Rest, Operations, Char, Acc);
strip_int(Binary, [leading|Rest], Char, Acc) ->  
    strip_int(Binary, Rest, Char, Acc );
strip_int(<<Char,Binary/binary>>, [trailing|_Rest] = Operations, Char, {Tmp,Final}) ->
    strip_int(Binary, Operations, Char, {<<Tmp/binary,Char>>,Final});
strip_int(<<OtherChar,Binary/binary>>, [trailing|_Rest] = Operations, Char, {Tmp,Final}) ->
    strip_int(Binary, Operations, Char, {<<>>, <<Final/binary, Tmp/binary, OtherChar>>});
strip_int(<<>>, [trailing|Rest], Char, Acc) ->
    strip_int(<<>>, Rest, Char, Acc);
strip_int(Binary, [], _Char, {_,Final}) ->
    <<Final/binary,Binary/binary>>.
