## makeCacheMatrix is a vector containing methods a matrix x, its inverse x_inv
## and a set of vectors to set and get the value of x and x_inv

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inv_matrix) x_inv <<- inv_matrix
    get_inv <- function() x_inv
    list(set = set,
         get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## cacheSolve returns the inverse of x. If the inverse has already been cached,
## the function returns the cached value. Otherwise it calculates, stores
## and returns the inverse matrix of x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_inv <- x$get_inv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data)
    x$set_inv(x_inv)
    x_inv
}
