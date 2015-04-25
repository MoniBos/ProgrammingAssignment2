## the first function 'makeCacheMatrix' gets the value of a matrix, then sets the inverse 
##of that matix and finally gets that inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  
  get <- function()                              ##get matrix
  setinverse <- function(solve) matrix <<- solve      ##set the inverse by using function Solve
  getinverse <- function() matrix                     ##get the inverse
  list(get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function 'cacheSolve' calculates the inverse of the matrix in 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  matrix <- x$getinverse()               ##here it checks if the inverse has already beeen calculated
  if(!is.null(matrix)) {                 ##and if so then it retrieves the inverse from the cache. 
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)             ##here the inverse is calculated if it hasn't before
  x$setinverse(matrix)
  matrix                                 ## Returns a matrix that is the inverse of 'x'
}
