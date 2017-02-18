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

class OfdmRx(WorkerProcess.WorkerProcess):
    
    
    def __init__(self):
        
        """
            Constructor for OFDM Radar RX module.
            
        """
        
        WorkerProcess.__init__(self)
        
        self.commandPath = "OfdmRx"
        self.inputPath = ""
        self.outputPath = ""
        self.symbolInputPath = ""
        self.symbolSize = 16
        self.dwellTime = 128
        self.dopplerSize = 8
        self.sc11 = False
        
    
    def startProgram(self):
        
        """
            Starts an OFDM Radar RX process using the supplied parameters.
        """
        
        commandPath = []
        
        if self.sc11:
            self._setArgument(commandPath, "sc11")
            
        self._setArgument(commandPath, "inputStream", self.inputPath)
        self._setArgument(commandPath, "outputStream", self.outputPath)
        self._setArgument(commandPath, "symbolStream", self.symbolInputPath)
        self._setArgument(commandPath, "symbolSize", self.symbolSize)
        self._setArgument(commandPath, "dwellInterval", self.dwellTime)
        self._setArgument(commandPath, "dopplerInterval", self.dopplerSize)
        
        self._args = commandPath
        
        WorkerProcess.startProgram(self)
        
