#!/bin/bash

#Copyright 2017 Robert Christian Taylor
#
#Licensed under the Apache License, Version 2.0 (the "License");
#you may not use this file except in compliance with the License.
#You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#Unless required by applicable law or agreed to in writing, software
#distributed under the License is distributed on an "AS IS" BASIS,
#WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#See the License for the specific language governing permissions and
#limitations under the License.


# Modify the parameters below to adjust the behavior of this script.
# By default, the output of the chirpRx program is the magnitude of the 
# complex correlation. Depending on your situation, you will most
# likely also want to change the input parameters to the bladeRF program.

# chirpradar.sh <outputPath> <bladeRF center freq>


echo "Chirp Based Radar using a Blade RF CLI front end"
echo "Written by Robert C. Taylor"

loopback="0"

yasdrrPath="./yasdrr"
txFIFOPath="./__txFifoPath"
rxFIFOPath="./__rxFifoPath"

startFrequency=-11000000.0
endFrequency=11000000.0
sampleRate=26000000
riseSamples=128
silenceLength=4096
repetitions=3
chirpWindow=Hamming
amplitude=0.03

test -f $yasdrrPath

if [ $? -ne 0 ] ; then
    echo "The yasdrr executable was not found."
    echo "Input Path: $yasdrrPath"
    exit -1
fi

test -f $txFIFOPath

if [ $? -ne 1 ] ; then
    echo "File already exists with the desired fifo name"
    echo "Wanted fifo path: $txFIFOPath"
    exit -1
fi

test -f $rxFIFOPath

if [ $? -ne 1 ] ; then
    echo "File already exists with the desired fifo name"
    echo "Wanted fifo path: $rxFIFOPath"
    exit -1
fi

if ! [ -x "$(command -v bladeRF-cli)" ] ; then
    echo "The bladerf cli utility must be installed"
    exit -1
fi


mkfifo $txFIFOPath
mkfifo $rxFIFOPath

$yasdrrPath --mode="chirpTx" --chirpWindow=$chirpWindow  --startFrequency=$startFrequency --endFrequency=$endFrequency --sampleRate=$sampleRate --riseSamples=$riseSamples --repetitions=$repetitions --amplitude=$amplitude --silenceLength=$silenceLength --signalOutputFormat=signed16 > $txFIFOPath &

if [ $loopback = 1 ] ; then
    cat $txFIFOPath > $rxFIFOPath &
else
    bladeRF-cli -e "rx config file=$rxFIFOPath format=bin n=$[$repetitions * ($silenceLength+$riseSamples)] timeout=60s; tx config file=$txFIFOPath format=bin timeout=60s; set samplerate $sampleRate; set bandwidth $sampleRate; set frequency rx $2; set frequency tx $2; set txvga1 -4; set txvga2 25; set lnagain 6; set rxvga1  30; set rxvga2  30; trigger j71-4 tx master; trigger j71-4 rx slave; rx start; tx start; rx; tx; trigger j71-4 tx fire; tx wait; rx wait;" &
fi

cat $rxFIFOPath | $yasdrrPath --mode="chirpRx" --startFrequency=$startFrequency --endFrequency=$endFrequency --sampleRate=$sampleRate --riseSamples=$riseSamples --silenceLength=$silenceLength --output=$1 --signalInputFormat=signed16 --chirpWindow=$chirpWindow --pulseWindow=$chirpWindow --outputAsMagnitude &

echo "Waiting for programs to finish..."

wait

rm $txFIFOPath
rm $rxFIFOPath




