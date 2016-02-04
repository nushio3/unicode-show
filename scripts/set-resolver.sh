#!/bin/bash

LATEST_VERSION=$(wget -O/dev/null https://www.stackage.org/$1 2>&1 |
                     grep "https://www.stackage.org/$1" |
                     tail -n1 |
                     sed "s/.*$1/$1/")
sed "s/^resolver: .*/resolver: ${LATEST_VERSION}/" stack.yaml
