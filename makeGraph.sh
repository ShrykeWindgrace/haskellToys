#!/usr/bin/env bash
echo 'run stack and dot; take all dependencies, even external'
stack dot --external | dot -Tpdf -o deps.pdf
