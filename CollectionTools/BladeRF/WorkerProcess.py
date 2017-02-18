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



import subprocess

class WorkerProcess:
    
    
    def __init__(self):
        
        """
            Constructor for Worker Process. Sets all variables to their 
            default state.
        """
        
        self.__processHandle = None
        self.returncode = None
        self.commandName = ""
        self._args = []
        
        
        
    def startProgram(self):
        
        """ 
            Starts the program
        """
        
        self.killProgram()
        self.returncode = None
            
        finalArgs = copy.copy(self._args)
        finalArgs.insert(0,self.commandPath)
        
        self.__processHandle = subprocess.Popen(finalArgs)

    def waitForProgram(self):
        
        """
            Does not return control until the blade rf program has terminated.
        """
        
        if self.__processHandle <> None:
            self.__processHandle.wait()
            self.returncode = self.__processHandle.returncode
            self.__processHandle = None
        
        return self.returncode
            
            
        
    def poll(self):
        
        """
            Checks if the Blade RF program is still running. If it is not,
            sets return code.
        """
        
        if self.__processHandle <> None:
            self.__processHandle.poll()
            if self.__processHandle.returncode <> None:
                self.returncode = self.__processHandle.returncode
                self.__processHandle = None
    
    def __safeToEndProgram(self):
        
        """
            Verfies if the program exists and is still running.
        
            Side Effects include setting processHandle to none it the process
            has terminated, and updating the return code on both the process 
            handle and this class.
            
        """
        
        if self.__processHandle <> None:
            self.__processHandle.poll()
            if self.__processHandle.returncode <> None:
                return True
            else:
                self.returncode = self.__processHandle.returncode
                self.__processHandle = None
            
        return False
    
    
    def terminateProgram(self):
        
        """
            Sends the BladeRF program the terminate signal and then pauses waiting
            for its execution to finish.
        """
        
        if self.__safeToEndProgram():
            self.__processHandle.terminate()
            return self.waitForProgram()
        
        return self.returncode
        
    
    def killProgram(self):
        
        """
            Sends the kill signal to the Blade RF program then waits for the 
            program to terminate.
            
        """
        
        if self.__safeToEndProgram():
                self.__processHandle.kill()
                return self.waitForProgram()
            
        return self.returncode 
    
    

    def _setArgument(self, commandList, argumentName, argumentValue = None):
    
        """
            Adds an argument to the command list.
            --<Argument Name> <Argument Value>
        """
    
        commandList.append("--"+argumentName)
        if argumentValue <> None:
            commandList.append(str(argumentValue))
