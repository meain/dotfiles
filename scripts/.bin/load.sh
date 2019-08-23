#!/bin/bash
LOAD=$(ps -A -o %cpu | awk '{s+=$1} END {print s}' | sed  's/\..*//')
if [ $LOAD -ge 150 ];
then
  echo "#[fg=red]$LOAD%#[fg=default]"
else
  echo "$LOAD%"
fi
