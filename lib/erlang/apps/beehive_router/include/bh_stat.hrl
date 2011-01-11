-type bh_counter()::integer().

-record(bh_stats, {
	hit::bh_counter() =0,
	sent::bh_counter()=0		
	}).


