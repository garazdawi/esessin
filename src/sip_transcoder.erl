%% @author Lukas Larsson <garazdawi@gmail.com>
%% @copyright 2010

-module(sip_transcoder).

-include("stq.hrl").

-export([decode/3, encode/2]).

%% @doc Decode a SIP binary to a STQ data structure
-spec decode(binary(), Opaque :: term(), Opts :: proplist()) ->
    {more, Opaque :: term()} | {ok, Data :: stq_opaque(), Rest :: binary()}.
decode(Bin, undefined, Opts) ->
    {ok, Bin, <<>>}.

%% @doc Encode a STQ data structure to a SIP binary
-spec encode(Data :: stq_opaque(), Opts :: proplist()) ->
    binary().
encode(Data, Opts) ->
    Data.
