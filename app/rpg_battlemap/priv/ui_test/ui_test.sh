#!/bin/bash

BASE_DIR=`dirname $0`

echo ""
echo "Starting Testacular Server (http://vojtajina.github.com/testacular)"
echo "-------------------------------------------------------------------"

echo $BASE_DIR
testacular start $BASE_DIR/testacular.conf.js $*
