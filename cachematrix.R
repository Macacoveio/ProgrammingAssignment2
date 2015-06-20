## This function is actually a list of functions to
## 1 - set a matrix
## 2 - get the values of a matrix
## 3 - set the inverse of that matrix
## 4 - get the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
      inv <- NULL
      set <- function(y) {
            m <<- y
            inv <<- NULL
      }
      get <- function() m
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the matrix created by makeCacheMatrix
## function above. If the inverse matrix has already been calculated, the
## function retrieves it from the cache

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- m$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- m$get()
      inv <- solve(data)
      m$setinv(inv)
      inv
}