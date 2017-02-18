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


import WorkerProcess

class OfdmTx(WorkerProcess.WorkerProcess):
    
    
    def __init__(self):
        
        """
            Constructor for the OFDM radar transmitter module.
        """
        
        self.commandPath = "ofdmTx"
        self.symbolOutputPath = ""
        self.symbolSize = 16
        self.deadTime = 128
        self.sc11 = False
        self.numberOfPulses = 8
        
        
    def startProgram(self):
        
        """
            Starts an OFDM radar Transmitter process.
        """
        
        commandPath = []
        
                
        if self.sc11:
            self._setArgument(commandPath, "sc11")
            
        self._setArgument(commandPath, "symbolOutputPath", self.symbolOutputPath)
        self._setArgument(commandPath, "symbolSize", self.symbolSize)
        self._setArgument(commandPath, "deadTime", self.deadTime)
        self._setArgument(commandPath, "numberOfFrames", self.numberOfPulses)
        self._setArgument(commandPath, "randomData")
        
        self._args = commandPath
        
        WorkerProcess.startProgram(self)
