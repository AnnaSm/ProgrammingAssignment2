## Part 1 - makeCacheMatrix function
## makeCacheMatrix - creates matrix object, that can cache its inverse. 
## set function - sets matrix and resets cached Inverse matrix
## get function - returns matrix
## setSolve - function saves values of the Invert martix
## getSolve - function returns values of cached Inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()  {
                x
        }
        setSolve <- function(solve) {
                m <<- solve
        }
        getSolve <- function() {
                m
        }
        list(set = set, get = get,
             setSolve= setSolve, 
             getSolve= getSolve)
}


## Part2 - cacheSolve function
## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## cacheSolve checks if the inverse has already been calculated.
## If yes, cacheSolve returns the inverse from the cache with comment "getting cached data".
## If not, cacheSolve calculates the inverse again.



cacheSolve <- function(x=matrix(), ...) {        
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setSolve(m)
        m
        
}

