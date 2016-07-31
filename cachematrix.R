
# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value
        # initialize to NULL
    invr <- NULL
            # create the matrix in the working environment
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
            # get the value of the matrix

    get <- function() x
            # invert the matrix and store in invr
    setinverse <- function(inverse) invr <<- inverse
            # get the inverted matrix from cache
    getinverse <- function() invr
            # return the created functions to the working environment
    list(set=set, 
    get=get, 
    setinverse=setinverse, 
    getinverse=getinverse)
}

# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## attempt to get the inverse of the matrix stored in cache
        invr <- x$getInverse()

        # return inverted matrix from cache if it exists
        # else create the matrix in working environment
        if (!is.null(invr)) {
                message("getting cached data")

                # display matrix in console
                return(invr)
        }

        # create matrix since it does not exist
        matrix <- x$get()

        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch( {
                # set and return inverse of matrix
                invr <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setMatrix(invr)
        } )

        # display matrix in console
        return (invr)
}
