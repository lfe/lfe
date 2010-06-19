#! /bin/bash
# Run LFE shell by default. Can add -pa if necessary.
erl "$@" -noshell -noinput -s lfe_boot start
