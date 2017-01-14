#!/bin/sh

Rscript spec-bottom-generator.r > spec.hs-bottom
echo "-- This file was generated using buildtest.sh" > spec.hs
cat spec.hs-top >> spec.hs
cat spec.hs-bottom >> spec.hs

