library(tidyverse)

testVector <- c(1,2,3,4,5,6)

evenVec <- function(x) {
  x[x %% 2 == 0]
}

evenVec(testVector)

lastVec <- function(x) {
  x[c(1:length(x) - 1)]
}

lastVec(testVector)

evenVecNoMissing <- function(x) {
  x[!is.na(x) & x %% 2 == 0]
}

evenVecNoMissing(testVector)

testVector
testVector[1:length(testVector) - 1]  
testVector[1]

df <- tibble(a = 1, b = 2, c = "a", d = "b", e = 4)

# First find numeric columns
num_cols <- sapply(df, is.numeric)
num_cols
#>     a     b     c     d     e 
#>  TRUE  TRUE FALSE FALSE  TRUE

df <- tibble(a = 1, b = 2, c = "a", d = "b", e = 4)
df[, ]