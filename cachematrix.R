## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # initialize the inverted matrix's value
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) { # check if previously calculated
    message("getting cached data")
    return(i) # if so, retrieve the caclulated matrix
  }
  data <- x$get() # if not previously calculated, go to else statement
  i <- solve(data, ...) # and calculate the inverse
  x$setinverse(i)
  i
}

# TEST FUNCTIONS
m1 <- matrix(rnorm(36), nrow = 6)
m1

m2 <- makeCacheMatrix(m1)
m2
cacheSolve(m2)
