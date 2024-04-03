# makeCacheMatrix() creates a vector (list) of functions to set/get the matrix
# and set/get the inverse matrix.
# setinverse caches the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() checks to see if the inverse matrix has already been 
## created and stored in the cache by makeCacheMatrix().
## If so, it retrieves the inverse matrix from the cache, otherwise it 
## calculates the inverse and sets the value of the inverse matrix in the cache 
## via setinverse.

cacheSolve <- function(x, ...) {
  ## Caches and returns an inverse matrix
  ## x (list): list of functions from makeCacheMatrix()
  
  ## check for existing inverse matrix
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## calculate matrix inverse and cache it
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
