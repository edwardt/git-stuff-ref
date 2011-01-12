PACKAGE_NAME = beehive
PACKAGE_VERSION = 0.1

.PHONY: deps compile rel test

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

check:
	@echo "Dependencies"
	@./rebar check-deps

clean:
	@./rebar clean
	@rm -rf ./rel/files/etc

rel: all
	@(cp -Rf ./etc ./rel/files/etc)
	@(make rel_erlang)
	@(chmod u+x ./rel/beehive/bin/beehive)
	@(make rel_message)

rel_erlang:
	@./rebar generate force=1

rel_message:
	@echo "\
Beehive code generated in `pwd`/rel/beehive\n\
*----------------------------------------------------------*\n\
* IMPORTANT                                                *\n\
*----------------------------------------------------------*\n\
To use git commit hooks properly, set the following value.\n\n\
code_root: `pwd`/rel/beehive\n\
\n\
This should be set in a beehive config file, found at\n\g
~/.beehive.conf or /etc/beehive.conf.  See docs for more info.\
\n\n\
This value will need to always reflect the current root of\n\
your beehive release.  Otherwise git commit hooks won't know\n\
where to find scripts to trigger app actions.\n\
*----------------------------------------------------------*"

doc:
	@./rebar doc skip_deps=true

package:
	@(mkdir -p ./builds)
	@(tar -C rel -c beehive | gzip > ./builds/${PACKAGE_NAME}-${PACKAGE_VERSION}.tar.gz)

test: deps compile
	@./test/bootstrap.sh
    ifdef suite
	@./rebar skip_deps=true eunit suite=$(suite)
    else
	@./rebar skip_deps=true eunit
    endif
