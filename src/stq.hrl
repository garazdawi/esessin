
%% General
-type proplist() :: list({atom(),term()}).

%% stq types
-type stq_opaque() :: term().
-type sip_body() :: binary().
-type sip_header() :: binary().
-type sip_header_field() :: atom() | binary().
-type sip_code() :: integer().
-type sip_method() :: atom() | binary().
-type sip_resp_msg() :: binary().
-type sip_uri() :: binary().
-type sip_vsn() :: {integer(), integer()}.

