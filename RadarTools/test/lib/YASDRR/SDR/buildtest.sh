#!/bin/sh
echo "--generated by buildtest.sh" > spec.hs-bottom
Rscript spec-bottom-generator.r >> spec.hs-bottom

cat spec.hs-top > spec.hs
cat spec.hs-bottom >> spec.hs