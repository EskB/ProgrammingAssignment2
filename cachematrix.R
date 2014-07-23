## This function caches the inverse value of x and creates a list of functions 
##get, setinverse, getinverse


makeCacheMatrix <- function(x = matrix()) {
          
          ## Initialize the inverse property
          
          
          
          ## set the matrix
          
          set <- function( matrix ) {
                    m <<- matrix
                    i <<- NULL
          }
          
          ## get the matrix
          
          get <- function() {
                    
                    ## Return the matrix
                    m
                    
          }
          
          ## set the inverse of the matrix
          
          setInverse <- function(inverse) {
                    i <<- inverse
          }
          
          
          ## inverse of the matrix
          
          getInverse <- function() {
                    ## Return the inverse property
                    i
                    
          }
          
          ## Return a list of the Procedures
          
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}


## This function cacheSolve() can calculate the inverse of the special "matrix" 
##with functions of makeCacheMatrix 

cacheSolve <- function(x, ...) {
          
          
          ## Return a matrix that is the inverse of 'x'
          
          m <- x$getInverse()
          
          ## Just return the inverse if its already set
          
          if( !is.null(m) ) {
                    message("getting cached data")
                    return(m)
          }
          
          ## to Get the matrix from our object
          
          data <- x$get()
          
          ## Calculate the inverse using matrix multiplication
          
          m <- solve(data) %*% data
          
          ## Set the inverse to the object
          
          x$setInverse(m)
          
          ## Return the matrix
          
          m
}
