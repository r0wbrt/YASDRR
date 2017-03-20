# YASDRR

YASDRR - Yet Another Software Defined Radio Radar.

YASDRR is a collection of libaries and executables, written primary in Haskell,
for generating and processing radar waveforms. As of the moment, the library 
supports chirp compression radar. Future integration
with a BladeRF based front end is under development. The eventual goal is to 
build up enough functionality to support Synthetic Aperture Based Radar.

## How To Build

To build the software, open the RadarTools directory, run cabal configure 
and then cabal build. The resulting utilities and libraries will then be 
placed in RadarTools/dist/build. As of the moment, cabal building is flaky 
because of depedency issues.

## References
Eaves, Jerry, and Edward Reedy. Principles of modern radar. Springer Science & Business Media, 2012.

Wikipedia contributors, "Chirp," Wikipedia, The Free Encyclopedia, https://en.wikipedia.org/w/index.php?title=Chirp&oldid=747594351 (accessed February 26, 2017).

Wikipedia contributors, "Chirp compression," Wikipedia, The Free Encyclopedia, https://en.wikipedia.org/w/index.php?title=Chirp_compression&oldid=760131952 (accessed February 26, 2017).






