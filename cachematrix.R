## Overall description of the functions:
## 
## makeCacheMatrix
## Defines a list of functions (in a vector) for handling the cached matrix  
## ("x") and its cached inverse ("xinv"). Creates the variables (named as 
## indicated) for caching the matrices in the environment of the function.
## 
## cacheSolve
## Returns the inverse of a matrix. Receives as input the vector of  
## functions created by makeCacheMatrix, determines if the inverse 
## had been calculated before; if so, gets the cached version, 
## otherwise, calculates the inverse using the solve function.
## Note: The input matrix is set using the set function created in
## MakeCacheMatrix
 
makeCacheMatrix <- function(x = matrix()) {
##
## Creates the following 4 functions to handle a cached matrix  
## ("x") and its cached inverse ("xinv"):
##
## set
## Receives as input the matrix to be inverted and copies it
## to the cached matrix "x" in the environment of the makeChacheMatrix 
## function. Initializes the inverse ("xinv") as NULL indicating that it 
## has not been calculated yet.
##
## get
## Retrieves the input matrix from the cached version in "x" copied by
## the set function.
##
## setinverse 
## Takes as input the calculated inverse and copies it in the cached 
## variable "xinv" for future use
##
## getinverse
## Retrieves the inverse from the cached area. A NULL values retrieved means
## that the inverse has not been calculated before.
##        
        xinv <- NULL
        set <- function(mymatrix) {
                x <<- mymatrix
                xinv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xinv <<- inverse
        getinverse <- function() xinv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(fvector, ...) {
##
## Return a matrix that is the inverse of "x".
## The matrix "x" is entered using the set function created by the 
## makeCacheMatrix function.
##
## The function retrieves the value of "xinv" from the cached area using the
## getinverse function; if the value retrieved is not null, the function 
## simply returns this as the inverse of the matrix. Otherwise, it retrieves
## the input matrix form the cached area using the get function, calculates 
## the inverse using the solve function, stores it in the cached area with the
## set inverse function, and returns the inverse.

        inverse <- fvector$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mymatrix <- fvector$get()
        inverse <- solve(mymatrix)
        fvector$setinverse(inverse)
        inverse
}
