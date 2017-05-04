## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    setm <- function(y) {
        x <<-y
        invm <<- NULL
    }
    getm <- function() { x }
    setinv <- function(sMatrix) invm <<- sMatrix
    getinv <- function() { invm }
    list(setm= setm,getm =getm, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    invm = x$getinv()
    
    if (!is.null(invm)){
        message("caching data..")
        return(invm)
    }
    
    data = x$getm()
    invm = solve(data, ...)
    
    x$setinv(invm)
    return(invm)
}
