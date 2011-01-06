#!/bin/sh -e

echo "Starting rack '$NAME' on '$PORT'"

export RACK_ENV=$DEPLOY_ENV

echo $$ > $PIDFILE
if [ -f config.ru ]; then
  exec rackup -s thin config.ru -p $PORT
fi
