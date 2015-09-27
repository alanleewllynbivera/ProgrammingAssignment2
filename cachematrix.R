##************************************************************************************************
##                                      Matrix Inversion
##************************************************************************************************
## The file contains functions to carry out matrix inversion. Program will take advantage
## of the scoping rules of the R language and are manipulated to preserve state inside 
## of an R object. As Matrix inversion calcuation involves costly computation, caching helps 
## to store and retrieve pre computed results rather than computing same inputs repeatedly.
## The following two functions are used to cache the inverse of a matrix.


## Function : makeCacheMatrix()
##       The function creates a "matrix" object that can cache its inverse. Operations involves:-
##              1. set the value of the matrix
##              2. get the value of the matrix
##              3. set the value of inverse of the matrix
##              4. get the value of inverse of the matrix
## Params x : 
##       Input matrix of which inverse to be computed.
## Return   :
##      List contaning functions to get/set matrix/inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Function : cacheSolve()
##      This function computes the inverse of an invertible matrix. 
##      If the inverse has already been calculated (and the matrix has not changed), 
##      it retrieves the inverse from the cache.
## Params x  : 
##      Input matrix of which inverse to be computed.
## Params ...:
##      Arguments inputted.
## Return    :
##      Inverse of the inputted matrix.

cacheSolve <- function(x, ...) {

        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                
                message("Data fetched from cache.")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data)
        x$setinverse(inverse_matrix)
        inverse_matrix
}
