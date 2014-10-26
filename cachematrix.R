## makeCacheMatrix creates a CacheMatrix object for a given matrix 'x'. The object carries a matrix data container for cached inverse matrix, and
## four functions for manipulating the original and the inverse matrices

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL     # Reset inverse matrix cache variable at the creation of object, i.e. prior to the first calculation of the inverse
  
  set<-function(y) {  # CacheMatrix object manipulation - Update the original matrix
    x <<- y           # Update the matrix to the new value y
    inverse <<- NULL  # Empty the inverse matrix cache
  }
  
  get<-function() x   # CacheMatrix object manipulation - Return the original matrix
  
  getinverse<-function() inverse  # CacheMatrix object manipulation - Return the cached inverse matrix
  
  setinverse<-function(y) inverse <<-y  # CacheMatrix object manipulation - Store the inverse matrix (called by the cache function)
  
  list(set=set, get=get, getinverse=getinverse, setinverse=setinverse) # Return the object containing the functions and storage variables
}


## cacheSolve function takes a Cac`heMatrix object 'x' and returns the inverse matrix to the original matrix. The inverse matrix is either 
## returned from the CacheMatrix object, or is calculated and stored first and then retured

cacheSolve <- function(x, ...) { 
  inverse <- x$getinverse()      # Get the currently stored inverse matrix
  if (is.null(inverse)) {      # If the matrix has been changed or the inverse has not yet been calculated, it is null ...
    inverse<-solve(x$get())    # ... then calculate the inverse matrix ...
    x$setinverse(inverse)      # ... and store it into the cache object 
  }       
  inverse                      # Return the cached inverse matrix - either the one just calculated or the one in the cache already
}
