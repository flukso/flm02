#!/bin/sh
cd `dirname $0`
exec erl -smp auto +K true -name api@flukso.net -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s webmachine -s erlrrd
