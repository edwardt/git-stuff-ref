-define (IDLE_TIMEOUT, 40000).
-define (MAX_HEADERS, 100).
-define (CONNECTION_TIMEOUT, timer:seconds(30)).
-define (NODE_CONNECT_TIMEOUT, timer:seconds(3)).

-define(Function, hd(element(2,element(2,catch erlang:error([]))))).

-type protocol():: 'http'  | 
		   'https' | 
		   'tcp' |
		   'udp' |
		   'http-alt'. %supported protocol

-define (http_ok_200, <<"">>).
-define (http_accepted_202, <<>>).
-define (http_bad_request_400, <<>>).
-define (http_unauthorized_401, <<>>).
-define (http_forbidden_403, <<>>).
-define (http_method_not_allowed_405, <<>>).
-define (http_request_timedout_408, <<>>).
-define (http_conflict_409, <<>>).
-define (http_gone_410, <<>>).
-define (http_request_entity_too_large_413, <<>>).
-define (http_unspported_media_type_415, <<>>).
-define (http_internal_server_error_500, <<>>).
-define (http_not_implemented_501, <<"HTTP/1.1 501 Not Implemented\r\n\r\n">>).
-define (http_service_unavailable_503, <<>>).








