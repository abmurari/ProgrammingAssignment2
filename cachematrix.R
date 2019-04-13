## Week 3 Programming Assignment to leverage caching to minimize 
## computation time & effort of complex calculations like matrix inversion
## GitHub URL: https://github.com/abmurari/ProgrammingAssignment2
## Name: Abhinav Murari
## Date: 14 Apr 2019

## Caching function to create a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) 
  {
    matrixInv <- NULL
    setMat <- function(y)
    {
      x <<- y                                     ## get value from parent env
      matrixInv <<- NULL
    }
    getMat <- function() x
    setInv <- function(solve) matrixInv <<- solve ## get value from parent env
    getInv <- function() matrixInv
    list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
  }


## Function that gets the inverse of the input matrix, unless the output 
## is already cached, in which case the cached output is displayed saving
## on computation power

cacheSolve <- function(x, ...) 
  {
    ## Return a matrix that is the inverse of 'x'
    matrixInv <- x$getInv()
    if(!is.null(matrixInv))
    {
      message("getting cached matrix inverse")
      return(matrixInv)
    }
    matrixData <- x$getMat()
    matrixInv <- solve(matrixData, ...)
    x$setInv(matrixInv)
    matrixInv
  }
