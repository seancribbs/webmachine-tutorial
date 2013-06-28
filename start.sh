#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -noshell -noinput -boot start_sasl -s reloader -s tweeter
