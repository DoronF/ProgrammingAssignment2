## The following 2 functions provide a way for caching the inverse of matrix

## makeCacheMatrix initiates a list of named "setters" and "getters" functions
## it holds two values: a matrix (x) and it's inverse (inverse)

makeCacheMatrix <- function(x = matrix()) {
    # initiate inverse matrix value as NULL.
    # when a new matrix is passed the inverse must reset or it will be wrong.
    inverse <- NULL
    
    set <- function(y) {
        # make sure we have a matrix
        if (!is.matrix(y)) {
            print("makeCacheMatrix accepts matrix only")
        }
        else {
            # initiate set function which take the matrix y and assign to x
            # reset the inverse.
            x <- y
            inverse <- NULL
        }
    }
    # passes back x
    get <- function() {
        # return NaN if x is not a matrix
        if (!is.matrix(x)) {
            NaN
        }
        else {
            x
        }
    }
    # set inverse by passing taking in the function solve from the parent 
    # environment 
    setInverse <- function(solve) inverse <<- solve
    # return the value of inverse
    getInverse <- function() inverse
    # assigning names to the list of functions so that we can use 
    # the $ sign to reference them
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## cachSolve takes a makeCacheMatrix as it's argument. it solves the inverse of 
## the matrix in makeCacheMatrix. it tries to retrieve a cached version first 
## if one available otherwise it generates it using solve, then cache it.
cacheSolve <- function(x) {
    ## get inverse from makeCacheMatrix object
    inverse <- x$getInverse()
    # if it is not NULL, it means that it is the cached version
    if (!is.null(inverse)) {
        message("getting cached data")
        # return the cached inverse matrix
        return(inverse)
    }
    # otherwise we need to generate it
    # get the matrix 
    data <- x$get()
    # generate inverse matrix 
    inverse <- solve(data)
    # set is back as cache
    x$setInverse(inverse)
    # return the inverse matrix
    return(inverse)
}
