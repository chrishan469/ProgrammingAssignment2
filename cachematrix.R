## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse to NULL
    inv <- NULL
    
    # setter for matrix
    set <- function(mtrx) {
        x <<- mtrx
        inv <<- NULL
    }
    
    # getter for matrix
    get <- function() {x}
    
    # setter for the inverse
    set.inverse <- function(setinv) inv <<- setinv
    
    # getter for the inverse
    get.inverse <- function() inv
    
    # returns a list of getter and setter functions
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# get the stored inverse
    inv <- x$get.inverse()
    # if it exists return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # else calculate, store and return it
    raw.x <- x$get()
    inv <- solve(raw.x, ...)
    x$set.inverse(inv)
    
    # returns the inverse
    inv

}
