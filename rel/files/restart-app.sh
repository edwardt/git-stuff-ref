#!/bin/sh

echo $1

# COPY PASTA ALERT!  from beehive

RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)

RUNNER_BASE_DIR=${RUNNER_SCRIPT_DIR%/*}
RUNNER_ETC_DIR=$RUNNER_BASE_DIR/etc

echo $RUNNER_ETC_DIR

NAME_ARG=`grep -e '-[s]*name' $RUNNER_ETC_DIR/vm.args | awk '{print $1}'`
if [ -z "$NAME_ARG" ]; then
    echo "vm.args needs to have either -name or -sname parameter."
    exit 1
fi

# Extract the target cookie
COOKIE_ARG=`grep -e '-setcookie' $RUNNER_ETC_DIR/vm.args`
if [ -z "$COOKIE_ARG" ]; then
    echo "vm.args needs to have a -setcookie parameter."
    exit 1
fi

erl $COOKIE_ARG -noinput -name rpc -run beehive-rpc restart_app $NAME_ARG $1
