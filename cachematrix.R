
## This code caches the inverse matrix calculation which can
## be a time consuming process.  The first function stores a
## matrix and caches its inverse.  The second function calculates
## the inverse of the matrix but first looks at the cache to see
## if the inverse has already been calculated.  If so - it gets
## the inverse. If not - it calculates the inverse.

## This first function stores a matrix and caches its inverse. 
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

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



## The second function calculates the inverse of the matrix,
## but first looks at the cache to see if the inverse has already 
## been calculated.  If so - it gets the inverse. 
## If not - it calculates the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

#test
x <- rbind(c(1,2),c(-2,1))
m <- makeCacheMatrix(x)
m$get()

cacheSolve(m)
cacheSolve(m)
