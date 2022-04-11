## Functions that will give the inverse of a Matrix.

Cache_Matrix <- function(x = matrix()) {
        # Show error when x is not a matrix.
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This will return the inverse of a matrix. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Gets cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
