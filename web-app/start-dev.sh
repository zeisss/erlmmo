#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname web -boot start_sasl -s reloader  -s ewc
