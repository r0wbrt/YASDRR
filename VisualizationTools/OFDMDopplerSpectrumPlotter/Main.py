#!/usr/bin/env python
#Copyright Robert C. Taylor, 2017. All Rights Reserved


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
import mmap
import os
import struct
import matplotlib.pyplot as plt
import numpy as np
import math

def main():
    
    #Set up the commandline options
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("-c", "--columns", action="store", type="int", dest="columns", help="Number of columns in the graph" )
    parser.add_option("-r", "--rows", action="store", type="int", dest="rows", help="Number of rows in the graph")
    parser.add_option("-s", "--source", action="store", type="string", dest="source", help="The file to read the graph data from")
    (options, args) = parser.parse_args()
    
    #Grab the values.
    columns = options.columns
    rows = options.rows 
    source = options.source
    
    #Map our source into memory
    fh = os.open(source, os.O_RDONLY)
    memoryMappedFile = mmap.mmap(fh, 0, prot = mmap.PROT_READ)
    
    
    x = np.zeros(shape=(rows, columns))
    y = np.zeros(shape=(rows, columns))
    z = np.zeros(shape=(rows, columns))
    
    for xPos in range(0,columns):
        for yPos in range(0,rows):
            x[yPos,xPos] = xPos
            y[yPos,xPos] = yPos
            
            data = memoryMappedFile.read(8)
            
            (real, imag) = struct.unpack('ff', data)
            
            
            z[yPos,xPos] = math.sqrt(real*real + imag*imag)
            
    
    plt.pcolormesh(x,y,z, cmap="PuBu_r", vmin=z.min(), vmax=z.max())
    plt.title("Radar Return")
    plt.axis([x.min(), x.max(), y.min(), y.max()])
    plt.colorbar()
    
    plt.show()
    
    memoryMappedFile.close()
    os.close(fh)
    
    
    
    
    
main()
