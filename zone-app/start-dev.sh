#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname zone -boot start_sasl -s reloader -s zone start_link
