## In order to solve and cache the inverse of a matrix we'll use two functions:
## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a special 'matrix' object with a list containing a function to:
## set the value of the matrix: set
## get the value of the matrix: get
## set the value of the inverse: set_inverse
## get the value of the inverse: get_inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set = set, get = get, set_inverse = set_inverse,
         get_inverse = get_inverse)

}


## cacheSolve calculates the inverse of the the special "matrix"
## created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {

    i <- x$get_inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$set_inverse(i)
    i
}
