## Caching the Inverse of a Matrix

## This function creates a list of matrixs that can cache a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        setM <- function(y){
                x <<- y
                mInv <<- NULL
        }
        getM <- function () x
        setmInv <- function(inverse) mInv <<- inverse
        getmInv <- function() mInv
        list(setM = setM, getM = getM, setmInv = setmInv, getmInv = getmInv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated, then cache the inverse from makeCacheMatrix

cacheSolve <- function(x, ...) {
        mInv <- x$getmInv()
        if(!is.null(mInv)){
                message("Getting cached data")
                return(mInv)
        }
        amatrix <- x$getM()
        mInv <- solve(amatrix)
        x$setmInv(mInv)
        mInv
}
