#!/usr/bin/env bash

# Execute this file with: ./run.sh
main() {
  # To run the reconstruction task discarding utterances containing new words,
  # set keep_all_variants="0". To always keep such utterances, set to "1", and
  # set to "0 1" to do both.
  local keep_all_variants="0 1"
  local types="l c"
  local children="Alex Ethan Lily Naima Violet William"
  local ages="1_6 2_0 2_6 3_0 3_6 4_0"

  for keep_all_variant in $keep_all_variants; do
    for type in $types; do
      for child in $children; do
        for age in $ages; do
          if [ $keep_all_variant == '1' ]; then
            ./CBLmodel.py --type=$type --child=$child --age=$age --keep-all
          else
            ./CBLmodel.py --type=$type --child=$child --age=$age
          fi
        done
      done
    done
  done
}

main "$@"
