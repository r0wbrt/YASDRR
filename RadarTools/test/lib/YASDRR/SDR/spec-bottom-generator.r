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


SIZES <- c(1,2,4,8,16,32)
LASTSIZE = 32
SHIFTS = 2

cat("{-|")
runif(1)

cat("--")
cat(.Random.seed)
cat("-} \r\n")

ofdmTestCase <- function(LENGTH, SHIFT) {


return = matrix(nrow = LENGTH, ncol = LENGTH)
impulse <- c(1,LENGTH)

for( i in 1:LENGTH) {
    for( j in 1:LENGTH) {
        return[i,j] = complex(real=runif(1,-1,1), imaginary=runif(1,-1,1))
    }
}

returnShifted = matrix(nrow = LENGTH, ncol = LENGTH)
for( i in 1:LENGTH) {
    for( j in 1:LENGTH) {
        returnShifted[i,j] = return[i,j] * complex(argument = ((-2.0 * 3.14159 * SHIFT * (i-1)) / LENGTH))
    }
}

 

for( i in 1:LENGTH) {
    impulse[i] = complex(real=runif(1,-1,1), imaginary = runif(1,-1,1))
}


complexCorrelate <- function(impulse, signal) {
    
    impulseConj <- sapply(impulse, Conj)
    result <- c(1:length(signal))
    
    for( i in 1:(length(signal))) {
    
        if(length(impulseConj) - 1 > (length(signal) - i)) {
        
           impM <- impulseConj[1:((length(signal) + 1) - i)]
           
         } else {
         
            impM <- impulseConj
         }

        result[i] <- sum(impM * signal[i:(length(impM) + i - 1)])
    }
    
    return(result)
}

returnCorrelated <- matrix(nrow = LENGTH, ncol = LENGTH)

for( i in 1:LENGTH) {    
    returnCorrelated[,i] <- complexCorrelate(impulse, returnShifted[,i])
}

returnCorrelatedDoppler <- matrix(nrow = LENGTH, ncol=LENGTH)

for(i in 1:LENGTH) {
    returnCorrelatedDoppler[i,] = fft(returnCorrelated[i,])
}

cat("(assertEqual \"Expected output was not produced by OFDM radar processor\" (all  (\\(a,b) -> compareComplex a b) (zip (processOfdmRadarReturnVFlat ")



cat("(VUB.fromList [")
for(i in 1:LENGTH) {
    cat("(")
    cat(Re(impulse[i]))
    cat(") :+ (")
    cat(Im(impulse[i]))
    cat(")")
    if(i != LENGTH) {
        cat(",")
    }
}
cat("]) (")

cat(SHIFT)

cat(") (VB.fromList ([")
for(i in 1:LENGTH) {
    cat("(VUB.fromList [")
    for(j in 1:LENGTH) {
        cat("(")
        cat(Re(return[j,i]))
        cat(") :+ (")
        cat(Im(return[j,i]))
        cat(")")
        if(j!=LENGTH) {
            cat(",")
        }
    }
    if(i != LENGTH) {
        cat("]) ,")
    } else { cat("])") }
}
cat("])")


cat(" )) ")


cat("(concat [")
for(i in 1:LENGTH) {
    cat("[")
    for(j in 1:LENGTH) {
        cat("(")
        cat(Re(returnCorrelatedDoppler[i,j]))
        cat(") :+ (")
        cat(Im(returnCorrelatedDoppler[i,j]))
        cat(")")
        if(j!=LENGTH) {
            cat(",")
        }
    }
    if(i != LENGTH) {
        cat("] ,")
    } else { cat("]") }
}
cat("])")

cat(")) True)")

}


dopplerTestCase <- function(LENGTH, VECTOR) {

cat("assertEqual \"Expected output was not produced for doppler length")
cat(LENGTH)
cat("\" (all (\\(a,b) -> compareComplex a b) (zip (") 

if(VECTOR)
{
cat(" processDopplerReturnFlattenV ")
} else {
cat(" processDopplerReturnFlatten ")
}

    return = matrix(nrow = LENGTH, ncol = LENGTH)

    for( i in 1:LENGTH) {
        for( j in 1:LENGTH) {
            return[i,j] = complex(real=runif(1,-1,1), imaginary=runif(1,-1,1))
        }
    }

    returnCorrelatedDoppler <- matrix(nrow = LENGTH, ncol=LENGTH)

    for(i in 1:LENGTH) {
        returnCorrelatedDoppler[i,] = fft(return[i,])
    }
    
    
    
    cat(" (")
    if(VECTOR)
    {
        cat("VB.fromList ")
    }
    cat("[")
    for(i in 1:LENGTH) {
        cat("(")
        
        if(VECTOR) {
            cat("VUB.fromList ")
        }
        
        cat("[")
        for(j in 1:LENGTH) {
            cat("(")
            cat(Re(return[j,i]))
            cat(") :+ (")
            cat(Im(return[j,i]))
            cat(")")
            if(j!=LENGTH) {
                cat(",")
            }
        }
        if(i != LENGTH) {
            cat("]) ,")
        } else { cat("])") }
    }
    cat("])")
    
cat(" ) ")

    
    cat("(concat [")
    for(i in 1:LENGTH) {
        cat("[")
        for(j in 1:LENGTH) {
            cat("(")
            cat(Re(returnCorrelatedDoppler[i,j]))
            cat(") :+ (")
            cat(Im(returnCorrelatedDoppler[i,j]))
            cat(")")
            if(j!=LENGTH) {
                cat(",")
            }
        }
        if(i != LENGTH) {
            cat("] ,")
        } else { cat("]") }
    }
    cat("])")

cat(")) True")
    
}




cat("ofdmDecodeTestCases = [")

for(j in SHIFTS) {
    for(i in SIZES) {
        cat("testCase \"Ofdm Radar return test size")
        cat(i)
        cat("\" ")
        ofdmTestCase(i,runif(1,-i,i))
        if(j!=SHIFTS || i!=LASTSIZE)
        {
            cat(",")
        }
    }
}

cat("]\r\n")



cat("listDopplerTestCases = [")

for(i in SIZES) {
    cat("testCase \"List doppler processing size")
    cat(i)
    cat("\" (")
    dopplerTestCase(i,FALSE)
    cat(")")
    if(i!=LASTSIZE)
    {
        cat(",")
    }
}

cat("]\r\n")



cat("vectorDopplerTestCases = [")

for(i in SIZES) {
    cat("testCase \"Vector doppler processing size")
    cat(i)
    cat("\" (")
    dopplerTestCase(i,TRUE)
    cat(")")
    if(i!=LASTSIZE)
    {
        cat(",")
    }
}

cat("]\r\n")


