
%% General
-type proplist() :: list({atom(),term()}).

%% stq types
-type stq_opaque() :: term().
-type sip_body() :: binary().
-type sip_header_line_no() :: integer() | undefined.
-type sip_header_value() :: binary().
-type sip_header_field() :: atom() | binary().
-type sip_header() :: {sip_header_field(),list({sip_header_value(),
						sip_header_line_no()})}.
-type sip_code() :: integer().
-type sip_method() :: atom() | binary().
-type sip_resp_msg() :: binary().
-type sip_uri() :: binary().
-type sip_vsn() :: {integer(), integer()}.

