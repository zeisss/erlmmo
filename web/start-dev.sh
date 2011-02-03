#!/bin/sh
cd `dirname $0`
exec erl -sname master -pa $PWD/ebin $PWD/../chat/ebin $PWD/deps/*/ebin $PWD/deps/*/deps/*/ebin -boot start_sasl -s reloader -s chat -s web
