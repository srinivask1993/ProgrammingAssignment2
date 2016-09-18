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
      inv <- x$getinverse()
      if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
}

# Sample example:
# > source("ProgrammingAssignment2/cachematrix.R")
# > file <- makeCacheMatrix(matrix(1:4, 2, 2))
# > file$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > file$getInverse()
# NULL
# > cacheSolve(file)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(file)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > file$getInverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > file$set(matrix(c(2, 4, 1, 3), 2, 2))
# > file$get()
#      [,1] [,2]
# [1,]    2    1
# [2,]    4    3
# > file$getInverse()
# NULL
# > cacheSolve(file)
#      [,1] [,2]
# [1,]  1.5 -0.5
# [2,] -2.0  1.0
# > cacheSolve(file)
# getting cached data
#      [,1] [,2]
# [1,]  1.5 -0.5
# [2,] -2.0  1.0
# > file$getinverse()
#      [,1] [,2]
# [1,]  1.5 -0.5
# [2,] -2.0  1.0

