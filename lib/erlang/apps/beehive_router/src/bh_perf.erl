-module(bh_perf).
-behaviour(gen_server).

-include("../include/bh_stat.hrl").

-export([start/0, start/1, stop/0, hit/1, sent/1, get/1]).

-export([init/1, terminate/2, code_change/3,
	handle_call/3, handle_cast/2, handle_info/2]).


start()->
	Config = ?defaultConfigSet,	
	start(config).

start(ConfigSet)->
	.

