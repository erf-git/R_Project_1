# Ethan R Feldman
# Project 2
# STAT 511

# 1.
top = function(data, m=4, n=4) {
  
  # Checks it at the beginning if it's a valid input
  if (is.null(data) || nrow(data)<m || ncol(data)<n){
    return("Invalid data passed")
  }
  
  return(data[1:m, 1:n])
  
}

# Test A
top(matrix(1:9, nrow = 3, ncol = 3), 2, 2)

# Test B
Name <- c("Tiff", "Bill", NA, "Jake", "Tia")
Age <- c(23, 41, 32, 58, 26)
top(data.frame(Name, Age), m=2, n=2)

# Test C
top(matrix(1:9, nrow = 3, ncol = 3), 1, 4)

# Test D
Name <- c("Tiff", "Bill", NA, "Jake", "Tia")
Age <- c(23, 41, 32, 58, 26)
top(data.frame(Name, Age), n=2)


# 2.
compFactorial = function(myNum=1) {
  
  if (myNum < 0) {
    return("x is negative")
    
  } else if (myNum == 0) {
    return(1)
    
  } else {
    
    myFac = myNum
    while (myNum > 1) {
      myNum = myNum - 1
      myFac = myFac * (myNum)
    }
    
    return(myFac)
  }
}

compFactorial(5)
compFactorial(8)
compFactorial(0)
compFactorial(-8)


# 3. 
sqdif = function(x=20, y=23) {
  
  try( return( (y-x)^2 ) )
}

sqdif("hi", "hehe")
sqdif("hi", 3)
sqdif(1, 3)
sqdif()


# 4. 
eThing = function(mystring="ee") {
  index = 1
  ecount = 0
  result = mystring
  while (ecount < 2 && index <= nchar(mystring)) {
    
    if ( substr(mystring, index, index) == "e" || substr(mystring, index, index) == "E") {
      ecount = ecount + 1
    }
    
    if (ecount >= 2) {
      result = substr(mystring, 1, index-1)
    }
    
    index = index + 1
    
  }
  return (result)
}

eThing("R fever")
eThing("beautiful")
eThing("ECCENTRIC")
eThing("E1Ab0rAte")
eThing("eeeeek!")


# 5. 
sampleT = function(data) {
  
  try ({
    x = mean(data)
    s = sd(data)
    n = length(data)
    
    lowerBound = x - (1.96 * (s / sqrt(n)) )
    upperBound = x + (1.96 * (s / sqrt(n)) )
    
    return ( list(x, c(lowerBound, upperBound)) )
  })
}

sampleT(c(1, 3, 4, 6, 11, 14, 17, 20, 22, 23))
sampleT(c(1, 1, 1, 6, 11, 14))
sampleT(c(3, 6, 3))
sampleT(c(1, 1, 1))
