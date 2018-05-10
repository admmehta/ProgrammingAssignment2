#The MatrixInversion computation Simplification via Caching

## The first function, makeCacheMatrix creates a special "matrix", which is contains a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data)
  x$setinverse(inv)
  inv
}


##Solution:
## x = rbind(c(10,20 ), c(20, 10))
##m = makeCacheMatrix(x)
## m$get()
##      [,1] [,2]
##[1,]   10   20
##[2,]   20   10

##cacheSolve(m)
##        [,1]        [,2]
##[1,] -0.03333333  0.06666667
##[2,]  0.06666667 -0.03333333
##Computes inverse
##cacheSolve(m)
##getting cached data
##        [,1]        [,2]
##[1,] -0.03333333  0.06666667
##[2,]  0.06666667 -0.03333333
##Directly caches it.