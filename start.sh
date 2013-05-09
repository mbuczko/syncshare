#!/bin/sh
erl -pa ebin deps/*/ebin -s lager -s syncshare \
	-eval "io:format(\"Point your browser at http://localhost:8080~n\")."
