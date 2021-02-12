## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL        # variable to hold the matrix inverse; initialized to NULL
  set <- function(y) {     ## function that sets the value of matrix
    x <<- y                # assigning value of y to x , where x is in different env. (parent env.)
    minverse <<- NULL      #on finding new matrix, reset minverse variable to NULL
  }
  get <- function() x      # function to get value of matrix; returns the matrix
  setinverse <- function(inverse) minverse <<- inverse   # assign value passed to function to minverse in parent env.
  getinverse <- function() minverse       # get matrix inverse when called
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse <- x$getinverse()  # store value of value returned from getinverse function
  if(!is.null(minverse)) {    # if value returned, cached data has inverse value, done,return that value
    message("getting cached data")
    return(minverse)
  }
  data <- x$get()                 # else find inverse 
  minverse <- solve(data, ...)   #if X is a square invertible matrix, then solve(X) returns its inverse.
  x$setinverse(minverse)
  minverse                      
}
