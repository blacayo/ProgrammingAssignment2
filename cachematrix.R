# makeCacheMatrix
# This Function creates a special "matrix" object that can cache its inverse.
# The object does not calculate the inverse, just saves it inside.
# Saves the matrix to variable x and its inverse to variable Inv in scope
# This is similar to Object Oriente Programming were we create an object and it properties, functions, etc
# The result or returned object, has a list of the methods of the object
# These are the methods:
## set: sets matrix or stores it in X and resets the cached inverse in Inv
## get: returns the matrix that was stored in x
## setInverse: saves the calculated Inverse via the solve function
## getInverse: returns the cached inverse value that was stored in Inv
## In addition while the assignment had the assumption of all of the matrix values to have an inverse, just in case
## I included an statement using the det function to evaluate if indeed the passed matrix has an inverse
## if it doesn't it errors out and provide the respective message via the stop function.

makeCacheMatrix <- function(x = matrix()) {
        
        
        ## Evaluates if the passed matrix "x" has an inverse
        if (det(x)==0)
        {
                stop("The Matrix passed does not have an inverse",call.=FALSE)
        }
        
        
        ## Clears the variable Inv to be able to later store the inverse matrix value
        Inv <- NULL
        
        ## sets the matrix and cleans/resets the cached inverse variable
        set <- function (y) {
                x <<- y
                Inv <<- NULL
        }
        
        ## returns the matrix
        get <- function() {
                x
        }
        
        ## calculates the Inverse Matrix and assigns/sets it on the Inv cache variable
        setInverse <- function(solve){
                Inv <<- solve
        }
        
        ## returns the cached inverse variable value
        getInverse <- function() {
                Inv
        }
        
        # Has the setting or list of methocs to be returned via the object
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
        
}



# This function works with the makeCacheMatrix function to get the value of the Inverse of the matrix argument
# First it will check if there is a cached value stored, if there is such a value then that is the one that is
# returned and provides the respective message. 
# If there is no cached value then it calculates the Inverse value of the matrix and caches is for future reference
# This is done via the methods of makeCachedMatrix, like getInverse and setInverse

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## Gets the stored Inverse value, it could be NULL or have an actual value if calculated previously
        Inv <- x$getInverse()
        
        ## Evaluates if the returned values is NULL, if it is then it means there is not a cached value as it was
        ## not calculated before. However if it not NULL then it returns the cached value with the respective
        ## message

        if (!is.null(Inv)){
                
                message("Getting the Inverse Value of the original Matrix that is Cached")
                return(Inv)
        }
        
        ## if there was no cached value, then it first recovers the actual matrix
        matriz <- x$get()
        
        ## it calculates the value of the inverse matrix and stores it in the variable Inv
        Inv <- solve(matriz,...)
        
        ## sets the value of the Inverse matrix into the cache
        x$setInverse(Inv)
        
        ## provides the message that the value displayed is the calculated inverse for the first time
        message("Presenting the Inverse value calculated for the first time, which is not cached")
        
        ## returns the inverse matrix value
        Inv
        
}
