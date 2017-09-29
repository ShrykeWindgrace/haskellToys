#!/usr/bin/env bash
find src -name '*.hs' | xargs graphmod -q -p | dot -Tpdf -o structure.pdf