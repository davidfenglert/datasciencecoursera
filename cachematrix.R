
## The two functions defined here work together to invert a square,
##   invertible matrix. The inverted matrix is cached so that the inversion
##   is performed only once. The original matrix and the inverted matrix
##   are preserved in matrix variables in the environment of the
##   defining function makeCacheMatrix.

## makeCacheMatrix function
## This function will output a list of four functions that will be called by
##   the cacheSolve function. 
## Using the superassignment operator, "<<-", the value of the input
##   matrix is assigned to a matrix object in the environment of this
##   defining function (the enclosing environment).
## In the same way a matrix object to contain the inverted matrix is
##   created with a NULL value.
## A function that calls the functions defined here will have access to
##   these matrix objects.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinvmat <- function(invmat) im <<- invmat
        getinvmat <- function() im
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}

## cacheSolve function
## The input to this function is a list of functions created with the
##   function makeCacheMatrix.
## This function will search the environment of the function makeCacheMatrix
##   to get the variable containing the inverted matrix.
## If the value of this matrix variable is not NULL, it will return the
##   preserved inverted matrix.
## If the value of this is NULL, it will search the environment of the
##   function makeCacheMatrix to get the preserved matrix, invert it with
##   the solve function, and assign the inverted matrix to the variable
##   containing the preserved inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        im <- x$getinvmat()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinvmat(im)
        im
}
