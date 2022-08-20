#!/bin/bash

if [[ "$#" == 0 ]]; then
    exec ghci -XPartialTypeSignatures -fno-warn-partial-type-signatures
fi

# This is needed to run the `main` function by default.
{ echo main ; cat - ; } | exec ghci "$@" -XPartialTypeSignatures -fno-warn-partial-type-signatures