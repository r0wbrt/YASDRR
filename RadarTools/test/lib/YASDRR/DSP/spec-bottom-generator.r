
cat("{-|")
runif(1)

cat("--")
cat(.Random.seed)
cat("-} \r\n")

SHIFTS = 4
SIZES = 4
MAXSIZE = 128



MSHIFTS = 4
MSIZES = 4
MMAXSIZE = 16


formatTestProperty <- function(propertName, testFunction, comma) {

    return(sprintf("(testCase \"%s\" %s)%s", propertName, testFunction, comma))
}

formatAssertEqual <- function(string, a, b) {

    return(sprintf("(assertEqual \"%s\" %s %s)", string, a, b))
}

formatComplexList <- function(list,vector) {

    if(vector) {
        buffer = "(VUB.fromList ["
    } else {
        buffer = "(["
    }

    for(i in 1:length(list)) {
        buffer = paste(buffer,formatComplexNumber(list[i]))
        if(i!=length(list)) {
            buffer = paste(buffer,",")
        }
    }
    
    buffer = paste(buffer,"])")
    return(buffer)
}

formatComplexNumber <- function(a) {
    return(sprintf("((%f) :+ (%f))", Re(a), Im(a)))
    
}

formatDoubleListCheck <- function(list1, list2) {
    return(sprintf("(all (id) $ zipWith compareComplexDouble %s %s)", list1, list2))
}

formatShiftFunction <- function(shift,size,signal,vector) {
    if(vector) {
        return(sprintf("(VUB.toList (cyclicShiftV (%f) (%d) %s))", shift, size, signal))
    } else {
        return(sprintf("(cyclicShift (%f) (%d) %s)", shift, size, signal))
    }

}

cyclicShiftTestCase <- function(shift,size,vector) {
    
    startInput <- c (1:size)
    for(i in 1:size) {
        startInput[i] <- complex(real=runif(1,-1,1), imaginary=runif(1,-1,1))
    }
    
    endInput <- c(1:size)
    for(i in 1:size) {
        endInput[i] <- startInput[i] * complex(argument = ((-2.0 * 3.14159 * shift * (i-1)) / size))
    }
    
    expectedBuffer = formatComplexList(endInput,FALSE)
    inputBuffer = formatShiftFunction(shift,size,formatComplexList(startInput, vector),vector)
    return(formatAssertEqual("Expected output was not recieved", "True", formatDoubleListCheck(inputBuffer, expectedBuffer)))

}


cyclicShiftListTestCases <- function(SHIFTS,SIZES,MAXSIZE,NAME,VECTOR) {

    buffer = sprintf("%s = [", NAME)

    for(shiftIdx in 1:SHIFTS) {
        for(sizeIdx in 1:SIZES) {
            size <- floor(runif(1,1,MAXSIZE))
            shift <- runif(1,-size,size)
            if(shiftIdx==SHIFTS && sizeIdx == SIZES) {
                comma = ""
            } else {
                comma = ","
            }
            buffer = paste(buffer,formatTestProperty(sprintf("Cyclic Shift %f test case with size %d",shift,size), cyclicShiftTestCase(shift,size,VECTOR), comma))
        }
    }
    buffer = paste(buffer,"]")
    return(buffer)
}


cyclicShiftMatrixTestCase <- function(shift,size,vector) {
    
    inputMatrix = matrix(nrow = size, ncol = size)
    
    for(i in 1:size) {
        for(j in 1:size) {
            inputMatrix[i,j] = complex(real=runif(1,-1,1), imaginary=runif(1,-1,1))
        }
    }

    outputMatrix = matrix(nrow = size, ncol = size)
    
    for(i in 1:size) {
        for(j in 1:size) {
            outputMatrix[i,j] = inputMatrix[i,j] * complex(argument = ((-2.0 * 3.14159 * shift * (i-1)) / size))
        }
    }

    
    inputBuffer = ""
    
    if(vector) {
        inputBuffer = "( VB.fromList ["
    } else {
        inputBuffer = "(["
    }
    
    for(i in 1:size) {
        inputBuffer = paste(inputBuffer, formatComplexList(inputMatrix[,i],vector))
        if(i != size) {
            inputBuffer = paste(inputBuffer, ",")
        }
    }

    inputBuffer = paste(inputBuffer,"])")
    
    outputBuffer = "(concat ["
    
    for(i in 1:size) {
        outputBuffer = paste(outputBuffer, formatComplexList(outputMatrix[,i], FALSE))
        if(i != size) {
            outputBuffer = paste(outputBuffer,",")
        }
    }
    
    outputBuffer = paste(outputBuffer, "])")
    
    inputFunc = ""
    
    if(vector) {
        inputFunc = sprintf("(concat $ map (VUB.toList) $ VB.toList (cyclicMutateMatrixV (%f) (%d) (%s)))", shift, size,inputBuffer)
    } else {
        inputFunc = sprintf("(concat (cyclicMutateMatrix (%f) (%d) (%s)))", shift, size,inputBuffer)
    }
    
    return(formatAssertEqual("Expected output was not recieved", "True", formatDoubleListCheck(inputFunc, outputBuffer)))
    
    
}

cyclicMatrixShiftTestCases <- function(SHIFTS,SIZES,MAXSIZE,NAME,VECTOR) {

    buffer = sprintf("%s = [", NAME)

    for(shiftIdx in 1:SHIFTS) {
        for(sizeIdx in 1:SIZES) {
            size <- floor(runif(1,1,MAXSIZE))
            shift <- runif(1,-size,size)
            if(shiftIdx==SHIFTS && sizeIdx == SIZES) {
                comma = ""
            } else {
                comma = ","
            }
            buffer = paste(buffer,formatTestProperty(sprintf("Cyclic Matrix Shift %f test case with size %d",shift,size), cyclicShiftMatrixTestCase(shift,size,VECTOR), comma))
        }
    }
    buffer = paste(buffer,"]")
    return(buffer)
}


cat(cyclicShiftListTestCases(SHIFTS,SIZES,MAXSIZE,"cyclicVectorTestCases",TRUE))
cat("\r\n")
cat(cyclicShiftListTestCases(SHIFTS,SIZES,MAXSIZE,"cyclicListTestCases",FALSE))
cat("\r\n")
cat(cyclicMatrixShiftTestCases(MSHIFTS,MSIZES,MMAXSIZE,"cyclicMatrixListTestCases",FALSE))
cat("\r\n")
cat(cyclicMatrixShiftTestCases(MSHIFTS,MSIZES,MMAXSIZE,"cyclicMatrixVectorTestCases",TRUE))

