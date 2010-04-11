#!/bin/sh
cd `dirname $0`
exec erl -name master -pa $PWD/ebin $PWD/deps/*/ebin $PWD/deps/*/deps/*/ebin -boot start_sasl -s reloader -s web
