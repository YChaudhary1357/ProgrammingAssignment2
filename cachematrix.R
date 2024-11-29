makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y) {
    x <<- y  
    inv <<- NULL 
  }

  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get() 
  inv <- solve(mat, ...) 
  x$setInverse(inv)  
  inv 
}

matrix_data <- matrix(c(1, 2, 3, 4), 2, 2,byrow = TRUE)
matrix_data
cached_matrix <- makeCacheMatrix(matrix_data)
cached_matrix$get()
cacheSolve(cached_matrix)
cacheSolve(cached_matrix)
cached_matrix$get()
