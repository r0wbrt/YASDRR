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

class BladeRF(WorkerProcess.WorkerProcess):
    
    #Constant representing 1.5 Mhz
    __onePointFiveMhz = 1500000
    
    #Center of the 2.4 Ghz ISM band. (2.45Ghz)
    __US2Point4GHZISMBandCenter = 2450000000
    
    def __init__(self):
        
        """
            Constructor which sets all of the classes values to their default
            state. Note, transmitting is disabled by default and the center 
            frequency defaults to 2.45Ghz which is the middle of the ISM band 
            in the USA.
        """
        
        WorkerProcess.__init__(self)
        
        self.enableRx = True
        self.enableTx = False
        self.rxOutputPath = ""
        self.txInputPath = ""
        self.sampleRate = __onePointFiveMhz
        self.filterSize = __onePointFiveMhz
        self.rxCenterFrequency = __US2Point4GHZISMBandCenter
        self.txCenterFrequency = __US2Point4GHZISMBandCenter
        self.paPower = 0
        self.rxSamplesToGrab = 10000
        self.lna = 0
        self.rxVga1 = 0
        self.rxVga2 = 0
        self.txVga1 = 0
        self.txVga2 = 0
        self.commandPath = "bladeRF-cli"
        
        
    
    def startProgram(self):
        
        """
            Starts the blade rf capture program using the options supplied.
        """
        
        self.__setOption(subprocessCommandString, "bandwidth", self.filterSize)
        self.__setOption(subprocessCommandString, "samplerate", self.sampleRate)

        if self.enableRx:
            self.__setOption(subprocessCommandString, "rxvga1", self.rxVga1)
            self.__setOption(subprocessCommandString, "rxvga2", self.rxVga2)
            self.__setOption(subprocessCommandString, "lnagain", self.lna)
            self.__setOption(subprocessCommandString, "frequency", self.rxCenterFrequency, "rx")
            self.__configModule(subprocessCommandString, self.rxOutputPath, self.rxSamplesToGrab, "rx") 
            self.__startModule("rx")
            
        if self.enableTx:
            self.__setOption(subprocessCommandString, "txvga1", self.txVga1)
            self.__setOption(subprocessCommandString, "txvga2", self.txVga2)
            self.__setOption(subprocessCommandString, "frequency", self.txCenterFrequency, "tx")
            self.__configModule(subprocessCommandString, self.txOutputPath, "", "tx") 
            self.__startModule("tx")
            
        
        self._args = subprocessCommandString
        
        WorkerProcess.startProgram(self)
        
        
    def __setOption(self, commandString, name, value, module=None):
        
        """
            Sets a single command option. set <name> <value>;
            If module is set, set <module> <name> <value>;
        """
        
        commandString.append("set")
        if module <> None:
            commandString.append(module)
        commandString.append(name)
        commandString.append(str(value) + ";")
        
        
    def __startModule(self, commandString, module):
        
        """
            Starts a module. Adds <module> start; <module> wait;
        """
        
        commandString.append(module)
        commandString.append("start;")
        commandString.append(module)
        commandString.append("wait;")
        
        
        
    def __configModule(self, commandString, filePath, samples, module=None):
        
        """
            Configs a module by setting up its file paths.
            
            If samples is not set,
            <module> config file <filepath> format=bin;
            
            Otherwise,
            <module> config file <filepath> n=<samples> format=bin;
        """
        
        commandString.append(module)
        commandString.append("config")
        commandString.append("file")
        commandString.append(filePath)
        
        if samples <> None:
            commandString.append("format=bin")
            commandString.append("n=" + str(samples) + ";")
        else:
            commandString.append("format=bin;")
        

        
    
