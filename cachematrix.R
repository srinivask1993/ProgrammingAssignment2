# The first function, makeCacheMatrix creates a special "matrix" which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function cachSolve, returns the inverse of the matrix. It first checks if
# the inverse has already been calculated. If so, it gets the inverse from the cache and
# skips the computation. If not, it computes the inverse, and sets the value in the cache 
# via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
