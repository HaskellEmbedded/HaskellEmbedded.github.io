#!/bin/sh

PANDOC_BIN="${HOME}/.cabal/bin/pandoc"
S5_PATH="../s5/default"

${PANDOC_BIN} -V s5-url:${S5_PATH} -t s5 -s Slides.md -o Slides.html 
