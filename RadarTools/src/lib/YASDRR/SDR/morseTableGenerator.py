#!/usr/bin/env python
#Copyright Robert C. Taylor, 2017. All Rights Reserved
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


import sys


def getSymbol(c):
    if c == ".":
        return "MorseDot"
    elif c == "-":
        return "MorseDash"
    else:
        raise Exception("Invalid character," + c)
    
def main():
    with open("MorseCodeTable.ref", 'r') as openfileobject:
        for line in openfileobject:
            
            sys.stdout.write(', (\'')
            
            line = line.replace("\r", "") 
            line = line.replace("\n", "") 
            
            for i, c in enumerate(line):
                if i == 0: 
                    sys.stdout.write(c)
                    sys.stdout.write('\',[')
                elif i + 1 == len(line):
                    sys.stdout.write(getSymbol(c))
                    sys.stdout.write(']')
                else:
                    sys.stdout.write(getSymbol(c))
                    sys.stdout.write(', ')
            sys.stdout.write(')\r\n')
                    

main()
