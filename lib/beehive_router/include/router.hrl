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




