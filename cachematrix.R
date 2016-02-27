## Matrix inversion can be a costly computation and there may be a benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## The below functions will cache and solve the inverse of a matrix.

## The first function, "makeCacheMatrix" will create a special "matrix" 
## object that can cache its inverse.

##This is compiled into a list containing a function to:
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {                   
                x <<- y
                invr <<- NULL
        }
        get <- function() x                                    
        setinverse <- function(inverse) invr <<- inverse        
        getinverse <- function() invr                             
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function "cacheSolve", will calculate the inverse of the cached 
## matrix created in "makeCacheMatrix" function.

cacheSolve <- function(x, ...) {
        invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(ins)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$setinverse(invr)
        invr
        ## Return a matrix that is the inverse of 'x'
}
