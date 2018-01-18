

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()){
  m <- NULL
  #Declare set in makeCacheMatrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Declare get in makeCacheMatrix
  get <- function() x
  #Declare setinverse in makeCacheMatrix
  #solve is a library function that can return inverse of a square matrix
  setinverse <- function(solve) m <<- solve
  #Declare getinverse in makeCacheMatrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  #Check if inverse is already available as cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
} 