
## The makeCacheMatrix function creates a matrix,that contains a function that sets and gets the value of the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
          inv <- NULL 
        set <- function(y) 
        { 
            x <<- y 
            inv <<- NULL 
         } 
        get <- function() x 
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv 
        list(set = set, get = get, setinv = setinv, getinv = getinv) 
}



## The cacheSolve function calculates the inverse of the matrix .
## It first checks to see if the inverse has already been calculated.
        ### If it has already been calculated then gets the inverse from the cache and skips the computation.
        ### If not, it calculates the inverse of the matrix with the setinv, setting the value of the inverse in the cache.

cacheSolve <- function(x, ...) 
{ 
        inv <- x$getinv() 
        if(!is.null(inv)) 
                { 
                  message("getting cached result") 
                  return(inv) 
                } 
        data <- x$get() 
        inv <- solve(data, ...) 
        x$setinv(inv) 
        inv 
} 
