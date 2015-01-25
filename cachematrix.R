
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv<-NULL
  # set the value of the matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  # get the value of the matrix
  get<-function() x
  # set the value of the inverse
  setinv<-function(solve) inv<<- solve
  # get the value of the inverse
  getinv<-function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  # Cache the inverse
  x$setinv(inv)
  # Return it
  inv
}

# Example usage:
  # Create a matrix x
  x <- matrix(c(1,0,5,2,1,6,3,4,0), nrow = 3, ncol=3)
  # Create the special matrix
  cx <- makeCacheMatrix(x)
  # Return the matrix
  cx$get()
  # Return the inverse
  cacheSolve(cx)
  # Call the function once more and get value from cache
  cacheSolve(cx)

