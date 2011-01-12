-type bh_counter_val()::non_neg_integer().
-type bh_stat_val():: number().
-type bh_counter_name()::atom().
-type stat_enable()::boolean().

-type protocol():: 'http' | 'https' | 'tcp' |'udp' |'http-alt'.

-define(Default_Conf, string:concat(erlang:atom_to_list(?MODULE),".conf")).


-record(bh_stats, {
	hit::bh_counter_val(),
	sent::bh_stat_val()		
	}).


