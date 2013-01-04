#!/bin/sh

APP_MODULE=saber_app
APP_DIR=`pwd`
NODE_NAME=$APP_MODULE@`hostname`

case "$1" in
    start)
        ./rebar compile skip_deps=true \
        && \
        erl --detached pa ebin -pa deps/*/ebin -name $NODE_NAME -s \
        $APP_MODULE start
        ;;
    console)
        ./rebar compile skip_deps=true \
        && \
        erl -pa ebin -pa deps/*/ebin -name $NODE_NAME -s $APP_MODULE start
        ;;
    reload)
        echo "TODO"
        ;;
    *)
        echo "Unknown command: $1"\
        exit 1
        ;;
    esac

exit 0
