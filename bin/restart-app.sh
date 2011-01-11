#!/bin/sh

RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)

NAME_ARG="beehive@127.0.0.1"
COOKIE_ARG="-setcookie beehive"

cd $RUNNER_SCRIPT_DIR

erl $COOKIE_ARG -noinput -name rpc@127.0.0.1 -run beehive-rpc restart_app $NAME_ARG $1
