
# makeCacheMatrix: This function creates a special 
# "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # Computing the inverse of a square matrix can 
  # be done with the solve function in R. 
  # 
  # For example, if X is a square invertible 
  # matrix, then solve(X) returns its inverse.
  # 
  # We assume that the matrix supplied is always invertible.



}


# cacheSolve: This function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}



