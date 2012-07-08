#!/bin/sh
basedir=BASE_DIR
ebin=${basedir}/ebin
deps=${basedir}/deps
conf=${basedir}/tend
erl -pa $ebin -env ERL_LIBS $deps -config $conf -s tend $*
