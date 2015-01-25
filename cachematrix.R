## These functions allow to calculate and cache the inverse of a given, inversible matrix.

## This function creates a special Matrix, which is a list containing a function to:
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      ## The inversibility of the matrix is not checked, as required.
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function calculates the inverse of a special matrix, created by the function above.
## It first checks to see if the inverse has already been calculated, and skips the calculation if so.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x', x being a special matrix
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
