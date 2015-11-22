## The functions within this R.script will create a Matrix that is cached and 
## and will to the inverse on the cache matrix

## The makeCacheMatrix will create a Cached Matrix

makeCacheMatrix <- function(x = matrix()) {
     cMatrix <- NULL
     set <- function(newMatrix) {
         x <<- newMatrix
         cMatrix <<-NULL
     }
     get <- function () x
     setCMatrix <- function(newCMatrix) cMatrix <<- newCMatrix
     getCMatrix <- function () cMatrix
     list(set=set, get=get,
          setCMatrix = setCMatrix, getCMatrix = getCMatrix)
}


## This function will cache the inverse of the cached matrix

cacheSolve <- function(x, ...) {
    iMatrix <- x$getCMatrix()
    if (!is.null(iMatrix)) {
        message("get cache inverse Matrix")
        return(iMatrix)
    }
    data <- x$get()
    if (nrow(data)==ncol(data)){
        iMatrix <- solve(data, ...)
        x$setCMatrix(iMatrix)
    } else {
        message("Matrix must be Square")
        return(NULL)
    }
    ## Return a matrix that is the inverse of 'x'
    iMatrix
}
