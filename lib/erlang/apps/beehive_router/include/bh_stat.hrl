-type bh_counter-val()::pos_integer().
-type bh_counter_name()::atom().

-record(bh_stats, {
	hit::bh_counter() =0,
	sent::bh_counter()=0		
	}).


