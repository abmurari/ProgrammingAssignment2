## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
  {
    matrixInv <- NULL
    setMat <- function(y)
    {
      x <<- y
      matrixInv <<- NULL
    }
    getMat <- function() x
    setInv <- function(solve) matrixInv <<- solve
    getInv <- function() matrixInv
    list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
  }


## Write a short comment describing this function

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
