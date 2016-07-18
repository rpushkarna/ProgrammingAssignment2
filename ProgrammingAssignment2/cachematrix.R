## makeCacheMatrix function creates a special vector which is basically a 
## a list of funcitons to create a matrix, set the value of matrix, create the 
## inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <<- NA
    set <- function(y){
        x <<- y
        inv <<- NA
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function that receives as an argument the special vector 
## and looks for the matrix values. If the matrix has not been changed returns 
## the inverse of the matrix from cache. In case no inverse is found, it solves the ## inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}