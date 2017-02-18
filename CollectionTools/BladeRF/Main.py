#!/usr/bin/env python
#   Copyright 2017 Robert C. Taylor
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


from optparse import OptionParser
import tempfile
import subprocess
import os
import BladeRF
import Morse
import OfdmRx
import OfdmTx


def main():
    
    #Set up the commandline options
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("", "--dwellInterval", action="store", type="int", dest="deadInterval", help="The amount of time to listen between transmissions" )
    parser.add_option("", "--symbolSize", action="store", type="int", dest="size", help="Number of samples used to compose a symbol")
    parser.add_option("", "--dopplerSize", action="store", type="int", dest="dopplerSize", help="Number of pulses to send to perform doppler processing")
    parser.add_option("", "--sampleRate", action="store", type="int", dest="sampleRate", help="Sampling rate used by both the transmitter and the receiver")
    parser.add_option("", "--ouput", action="store", type="string", dest="outputPath", help="File to store the resulting capture to")
    parser.add_option("", "--filter", action="store", type="string", dest="filter", help="Bandwidth of the front end receiver")
    parser.add_option("", "--frequency", action="store", type="string", dest="frequency", help="Center Frequency to transmit and receive on")
    parser.add_option("", "--PAPower", action="store", type="int", dest="PAPower", help="Amplification to use by the PA amplifier")
    parser.add_option("", "--LNA", action="store", type="int", dest="LNA", help="Amplification to use by the LNA amplifier")
    parser.add_option("", "--RXVGA1", action="store", type="int", dest="RXVGA1", help="Amplification to use by the RX VGA1 Amplifier")
    parser.add_option("", "--RXVGA2", action="store", type="int", dest="RXVGA2", help="Amplification to use by the RX VGA2 Amplifier")
    parser.add_option("", "--TXVGA1", action="store", type="int", dest="TXVGA1", help="Amplification to use by the TX VGA 1 Amplifier")
    parser.add_option("", "--TXVGA2", action="store", type="int", dest="TXVGA2", help="Amplification to use by the TX VGA 2 Amplifier")
    parser.add_option("", "--callSign", action="store", type="string", dest="callSign", help="Identifying call sign to send before and after the radar transmission")
    
    (options, args) = parser.parse_args()
    
    #tempDir = tempfile.mkdtemp()
    #morsePipePath = os.path.join(tempDir, "morseOutput")
    #os.mkfifo(morsePipePath)
    #morseOutputPipe = open(morsePipePath, "w+b")
    
    
    #morse = subprocess.Popen(["./morse", "--inputMessage", "Test OFDM Radar ranging by " + options.callSign, "--sampleRate", str(options.sampleRate), "--wpm", "20", "--frequency", "600", "--sc11"], stdout=morseOutputPipe)
    #subprocess.call(["cat", morsePipePath], stdout=devNull)
    #morse.wait()
    #morse.kill()
    ##Generate Morse File
    ## morse --inputMessage="Test OFDM Radar ranging by <Callsign>" --sampleRate=<sampleRate> --wpm="20" --frequency=600 --outputPath=morseCode.dat --SC11
    ##Open Transmitter and generate file
    ## tx --SignalOutput="tx.dat" --symbolOutput="symbols.dat" --symbolSize="<symbol size>" --deadTime=<dwellInterval> --sc11 --randomData --numberOfFrames=<dopplersize>
    ##Call Blade RF, write output to temp file and read input from generated file
    ##Call Blade RF, transmit morse code
    ##parse output
    ## rx --inputStream="capturedData.dat" --outputStream="finalResult.dat" --symbolStream="symbols.dat" --symbolSize="<symbol size>" --dwellInterval="<dwell interval>" --dopplerInterval=<doppler size> --sc11
    ##Clean up files
    ##Save final output, put inside of a tar gz file


main()


