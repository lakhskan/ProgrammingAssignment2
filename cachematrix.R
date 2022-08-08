## This function caches the inverse of a square matrix. Function assumes that we should supply only square and inversible matrix. 
## There is no check to determine if a matrix can be inversed.

## The function does the following:
## 1. set the object x to the matrix form
## 2. Initializes the inv (matrix inverse) to null
## 3. Set function to change the cached value of matrix
## 4. get function to access the matrix
## 5. getinv(erse) to get the inverse of the matrix
## 6. setinv(erse) so that cachesolve to access object inv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of the matrix obtained from the function makeCacheMatrix
## This function checks if the inverse of the matrix already computed and retrievs from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        input <- x$get()
        inv <- solve(input, ...)
        x$setinv(inv)
        inv
}
