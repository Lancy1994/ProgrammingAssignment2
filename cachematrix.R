## This function allows precalculated matrix inverse to be stored in memory - and not be recalculated repeatedly.
## This is time-saving. 


makeCacheMatrix <- function(x = matrix()) {
        
		## This function creates a special "matrix" object that can cache its inverse.
        
        inv <- NULL
        
        set <- function(y) 
        { x <<- y    
        inv <<- NULL     
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse;
        
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function first checks to see if the inverse matrix has already been calculated. 
## If so, it `get`s the inverse matrix from the cache and skips the computation. 
## Otherwise, it do calculations.

cacheSolve <- function(x, ...) {
        ## Calculate the inverse of 'x'
        inv <- x$getinv()
        print(x)
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
        inv
}