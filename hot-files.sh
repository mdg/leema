#! /usr/bin/env sh

NCOMMITS=50
NFILES=20

git diff --stat HEAD~$NCOMMITS . \
    | grep -F "src/leema/" \
    | sed -r "s/^ src\/leema\/(\w+\.rs) *\| *([0-9]+) \+*\-*$/\2 \1/" \
    | sort -nr \
    | head -$NFILES \
    | sed -r "s/[0-9]+ //"
