-type bh_counter_val()::non_neg_integer().
-type bh_stat_val():: number().

-type bh_counter_name()::atom().

-record(bh_stats, {
	hit::bh_counter_val(),
	sent::bh_stat_val()		
	}).


