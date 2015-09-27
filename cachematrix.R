## Functins to cache inverse of a matirx to save computations

## Function to make a special matrix which stores matrix and its inverse and 
## has methods get, set and getinverse and setinverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    set <- function(y) {
        x <<- y
        inv <<- matrix()
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function gets a inverse of a specialmatrix if its already calculated else
## it calculates is and sets it

cacheSolve <- function(x, ...) {
    ##  a matrix that is the inverse of 'x'
    inv <- x$getinverse()
        if(!anyNA(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse (inv)
        inv
}
