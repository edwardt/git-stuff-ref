-type bh_counter_val()::non_neg_integer().
-type bh_stat_val():: number().
-type bh_counter_name()::atom().

-define (default_conf, string:concat(erlang:atom_to_list(?MODULE),".conf").


-record(bh_stats, {
	hit::bh_counter_val(),
	sent::bh_stat_val()		
	}).


