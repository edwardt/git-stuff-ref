-type counter_val()::non_neg_integer(). %hit counter value

-type stat_val():: number().  %collected statistic value

-type counter_name()::atom(). %counter name

-type stat_enable()::boolean(). %whether to enable that stat collection or not

-type timestamp() :: non_neg_integer().  % microseconds with a special epoch



-record (counter, 
	{name::counter_name(), 
	 increment = undefined
	 }).
	 
-record (rate,
	{
	
	}).

-record(bh_stats, {
	hit::counter_val(),
	sent::stat_val()		
	}).


-define(Default_Conf, string:concat(erlang:atom_to_list(?MODULE),".conf")).
