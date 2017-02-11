# OFDMRadar

[![Build Status](https://travis-ci.org/r0wbrt/OFDMRadar.svg?branch=master)](https://travis-ci.org/r0wbrt/OFDMRadar)

Functionality to transmit and receive OFDM radar signals ans process them. This 
project is primaraly written in Haskell with the FFT used by the project
written in C. Python and R used to automate some of the development tasks.

Python is also used to automate the capturing of radar signals, processing it,
and visualizing it. Though, python is not needed to build the actual OFDM Radar
processing tools and associated library. 

## How To Build

To build the software, open the RadarTools directory, run cabal configure 
and then cabal build. The resulting utilities and libraries will then be 
placed in RadarTools/dist/build. 

## References
Tigrek, Recep Firat. "A processing technique for OFDM-modulated wideband radar 
signals." PhD diss., TU Delft, Delft University of Technology, 2010.

# Patents
This software may be covered by US patent US8081105B2. I am not a lawyer though
so I can not render a legal statement on this subject.





