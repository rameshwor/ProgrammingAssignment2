## Put comments here that give an overall description of what your
## functions do



## Function     : makeCacheMatrix
## Description  : This function when supplied with a matrix, returns the list of getter and setter functions for the matrix and its Inverse.


makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL;
        
        ## getter & setter for the matrix
        get <- function() {x} 
        set <- function(y) {
                x <<- y 
                invMatrix <<- NULL
        }
        
        ## getter & setter for the Inverse
        
        getInverse <- function() { invMatrix }
        setInverse <- function( Inv ) { invMatrix <<- Inv }
        
        # return the list of accessors & mutators
                
        list(get=get , set=set , getInverse=getInverse , setInverse=setInverse)
                

}


## Function : cacheSolve
## Description : This function when called on an object of type makeCacheMatrix returns the inverse of the matrix within the object 
##               makeCacheMatrix. If the inverse matrix is already computed, it is retrieved from the cache. 
##               If matrix isn't already computed and stored in cache, this function calculates the inverse and uses the mutators from 
##               makeCacheMatrix to set the inverse.
##               The inverse matrix is then returned using getter from the makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        
        A <- x$getInverse()
        if ( !is.null(A)) { 
                print(" Found in cache")
                return(A)
        }
        
        ## x$setInverse(solve(x$get()))
        Inv <- solve(x$get())
        x$setInverse(Inv)
        x$getInverse()
        
}
