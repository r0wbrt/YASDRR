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

class Morse(WorkerProcess.WorkerProcess):
    
    
    def __init__(self):
        
        WorkerProcess.__init__(self)
        
        self.wpm = 20
        self.toneFrequency = 600
        self.sampleRate = 44100
        self.sc11 = False
        self.message = ""
        self.inputFilePath = ""
        self.commandPath = "morse"
    
    
    def startProgram(self):
        
        """
            Starts the Morse program using the supplied arguments.
        """
        
        commandList = []
        
        self._setArgument(commandList, "wpm", self.wpm)
        self._setArgument(commandList, "frequency", self.toneFrequency)
        self._setArgument(commandList, "sampleRate", self.sampleRate)
        
        
        if self.inputFilePath <> "":
            self._setArgument(commandList, "inputFile", self.inputFilePath)
        
        if self.message <> "":
            self._setArgument(commandList, "inputMessage", self.message)
        
        if self.sc11:
            self._setArgument(commandList, "sc11")
        
        self._args = commandList
        
        WorkerProcess.startProgram()
    
    
    

