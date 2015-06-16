# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

#Test Cases 1
# set.seed(10)
# r = rnorm (16)
# r
# mm = matrix(r,4,4)
# mm
# mc = makeCacheMatrix(mm)
# cacheSolve(mc)
# cacheSolve(mc)

#Test Cases 2
# set.seed(10)
# r = rnorm (16, 2,2)
# r
# mm = matrix(r,4,4)
# mm
# mc = makeCacheMatrix(mm)
# cacheSolve(mc)
# cacheSolve(mc)
