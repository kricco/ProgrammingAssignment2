#define makeChacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
#define functions to set the value of the vector, get the value of the vector, set the value of inverse matrix and get the value of inverse matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
#create the special "vector" to store the list of functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
#define cacheSolve function
cacheSolve <- function(x, ...) {
#check if the value has been calculated before, if yes, get the cached data and return its value ie inverse matrix
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
#if the value has not been calculated before, do the calculation and return the inverse matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
