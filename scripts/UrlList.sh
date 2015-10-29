#!/bin/bash

echo -e ":l scripts/UrlList.hs\n:set args ${1:-\$1}\nmain\n:quit\n" | cabal repl
# cat /tmp/urls.txt

