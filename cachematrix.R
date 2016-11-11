## The following function will calculate the inverse of a matrix

## This function creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) { ##this is the main difference between the Caching the mean of a function.  Instead of x=numeric(), x=matrix.
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) inv <<-inverse
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse=setinverse,
                     getinverse=getinverse)
        }


## This part of the function computes the inverse of the matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <-solve(data)
        x$setinverse(inverse)
        inverse
}
        ## Return a matrix that is the inverse of 'x'
##Testing the cacheMatrix function
##>x=rbind(c(1,2),c(2,1)) ##this creates a simple matrix
##      [,1] [,2]
##[1,]    1    2
##[2,]    2    1
##>matrx=makeCacheMatrix(x)
##>matrx$get()
## When you run the inverse, you will receive no message about cached data because it is not commited to memory
##>cacheSolve(matrx)
##> cacheSolve(matrx)
##          [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
## Running the cacheSolve(matrx) the second time, checks to see if the inverse is identical and cached.
##> cacheSolve(matrx)
##getting cached data
##      [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333