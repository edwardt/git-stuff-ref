#!/bin/sh

# Compile
make compile

# We'll start a dummy gem server, if `gem` is present
GEM_BIN=`which gem`

if [ ! -z "$GEM_BIN" ]; then
  if [ -z "`ps aux | grep gem | grep server`" ]; then
    gem server >/dev/null 2>&1 &
  fi
fi

# Make root directory
if [ -z "$BEEHIVE_HOME" ]; then
  export BEEHIVE_HOME=/tmp/beehive
fi

if [ -z "$BEEHIVE_DOMAIN" ]; then
  export BEEHIVE_DOMAIN=`hostname -f`
fi

if [ -z "$BEEHIVE_REPOSITORY"]; then
  export BEEHIVE_REPOSITORY=local_git
fi

mkdir -p $BEEHIVE_HOME

# Start Beehive
echo "Starting beehive"
eval "erl \
    -setcookie beehive \
    -name beehive@127.0.0.1 \
    -pa deps/*/ebin -pa lib/*/ebin -pa lib/*/include  \
    -beehive database_dir '\"$BEEHIVE_HOME/db\"' \
    -eval \"application:start(sasl, [{errlog_type, error}])\" \
    -eval \"application:start(os_mon)\" \
    -eval \"application:start(crypto)\" \
		-s reloader \
    -eval \"application:start(beehive)\""
