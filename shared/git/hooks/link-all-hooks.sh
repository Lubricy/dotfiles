#!/usr/bin/env bash

for h in $(awk -F '"' '/^.SS/{gsub(/\\/, "", $2); print $2}' $(man --path githooks)); do
    ln -s ${1:-default} $h
done
