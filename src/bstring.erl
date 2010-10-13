%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(bstring).

%% API
-export([to_lower/1, to_upper/1, to_integer/1]).

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
