## Create a matrix object that is extended with the ability to cache its inverse
## Inverse is created by using the underlying solve function

## Create a cacheable matrix object, Remember only nonsingular square matrices are solvable by the solve method! 
## (http://mathworld.wolfram.com/NonsingularMatrix.html) 
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y) {
                x <<- y
                ##Clear the cached inversed value when setting new matrix
                s <<- NULL
        }
        
        ##Get the original matrix
        get <- function() x 
        
        ##Set the inverse of this matrix and store it in "cache"
        setsolve <- function(solve) { 
                        s <<- solve
        }  
        
        ##Return the previously cached matrix inverse (Returns NULL if value has not already been cache)
        getsolve <- function() {
                        s 
        }
        
        ##Return a list object with all the methods (can be called in the form x$get x$set etc.)
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Inverts a matrix created with the makeCacheMatrix object and caches the result in said object
## This method uses the underlying solve function, Remember only nonsingular square matrices are solvable by the solve method! 
## (http://mathworld.wolfram.com/NonsingularMatrix.html) 

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) { ##If the inverse method has already been cached return it
                message("Returning cached value")
                return(s)
        }
        ##Not cached, solve it and cache it!
        data <- x$get()  
        x$setsolve(solve(data, ...)) ##Inverse the matrix and cache it for future use
        x$getsolve() ##return the inverted matrix from cache 
}
