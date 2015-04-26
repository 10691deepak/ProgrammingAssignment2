## This assignment will write a pair of functions that will catch the inverse of a matrix
## The purpose of the assignment is to come up with code to caching the inverse of matrix
## Rather than trying to come up with inverse of matrix repeatedly

## The below function will create a special matrix object that could cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        inv_x <- NULL
        set <- function(y) {
            x <<- y
            inv_x <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv_x <<- inverse
        get_inverse <- function() inv_x
        list(set = set, get = get,
             set_inverse = set_inverse
             get_inverse = get_inverse)
}

## This function computes the inverse of the matrix returend by makeCacheMatrix
## If the cached inverse is available, the below function retrives it
## On the other hand, if cached inverse is not available, the the below function will compute, then cache and return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if (!is.null(inv_x)) {
           message("This will return Inverse of Cached Matrix")
           return(inv_x)
        } else {
           inv_x <- solve(x$get())
           x$set_inverse(inv_x)
           return(inv_x)
        }
}


