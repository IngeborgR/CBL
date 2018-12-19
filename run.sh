#!/usr/bin/env bash

# Execute this file with: ./run.sh
main() {
  local types="l c"
  local children="Alex Ethan Lily Naima Violet William"
  local ages="1_6 2_0 2_6 3_0 3_6 4_0"

  for type in $types; do
    for child in $children; do
      for age in $ages; do
        ./CBLmodel.py --type=$type --child=$child --age=$age
      done
    done
  done
}

main "$@"
