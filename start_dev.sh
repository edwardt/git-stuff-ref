#!/bin/sh
#cd `dirname $0`
if [ -z $BH_CODE_ROOT ]; then
    echo "WARNING!!!\nBad things will probably happen if you try to deploy
repos without setting the following value.\n\n"
    echo "export BH_CODE_ROOT=`pwd`\n\n"
    echo "exiting..."
    exit 1
elif [ $BH_CODE_ROOT != `pwd` ]; then
    echo "ACK! WARNING!!!"
    echo "If you're trying to use repos, terrible things may or may not happen,
because your BH_CODE_ROOT isn\'t equal to your current directory\n\n"
    echo "export BH_CODE_ROOT=`pwd`\n\n"
    echo "exiting..."
    exit 1
fi

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

if [ -z "$BEEHIVE_REPOSITORY"]
then
    export BEEHIVE_REPOSITORY=local_git
fi

mkdir -p $BEEHIVE_HOME

# Start Beehive
echo "Starting beehive"
eval "erl \
    -name beehive@127.0.0.1 \
    -pa deps/*/ebin -pa lib/erlang/apps/*/ebin -pa lib/erlang/apps/*/include  \
    -beehive database_dir '\"$BEEHIVE_HOME/db\"' \
    -eval \"application:start(sasl)\" \
    -eval \"application:start(os_mon)\" \
    -eval \"application:start(crypto)\" \
		-s reloader \
    -eval \"application:start(beehive)\""
