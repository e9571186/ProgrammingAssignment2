
####################################################
# makeCacheMatrix: This function creates a special 
# "matrix" object that can cache its inverse.
####################################################

makeCacheMatrix <- function(x = matrix()) {

  # initialize inverse matrix
  i <- NULL

  # set the value of the matrix
  set <- function(y) {
        x <<- y
        i <<- NULL
  }

  # get the value of the matrix
  get <- function() { 
     x
  }

  # set the value of the inverse matrix
  setinverse <- function(inverse) {
    i <<- inverse
  }

  # get the value of the inverse matrix
  getinverse <- function() { 
    i
  }

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}
####################################################


####################################################
# cacheSolve: This function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve 
# the inverse from the cache.
####################################################

cacheSolve <- function(x, ...) {

   # return a matrix that is the inverse of 'x'
   m <- x$getinverse()

   # return the cached data if possible 
   if(!is.null(m)) {
      message("getting cached data")
      return(x)
   }

   data <- x$get()

   ####################################################
   # computing the inverse of a square matrix can 
   # be done with the solve function in R. 
   # 
   # for example, if x is a square invertible 
   # matrix, then solve(x) returns its inverse.
   # 
   # we assume that the matrix supplied is always invertible.
   # 
   # http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
   # 
   # 
   # example:
   # > c = rbind(c(1, -1/4), c(-1/4, 1))
   # > c
   #       [,1]  [,2]
   # [1,]  1.00 -0.25
   # [2,] -0.25  1.00
   # 
   # > solve(c)
   #           [,1]      [,2]
   # [1,] 1.0666667 0.2666667
   # [2,] 0.2666667 1.0666667
   # 
   # > inverse <- solve(c)
   # 
   # matrix multiplication is denoted with %*%
   # without the % sign an element by element multiplication would
   # be excecuted
   #
   # > identitymatrix <- c %*% inverse
   # > identitymatrix  
   #      [,1] [,2]
   # [1,]    1    0
   # [2,]    0    1
   # 
   # > identitymatrix <- inverse %*% c
   # > identitymatrix
   #      [,1] [,2]
   # [1,]    1    0
   # [2,]    0    1

   m <- solve(data, ...)
   ####################################################

   # set the inverse
   x$setinverse(m)

   # return the matrix m
   m

}
####################################################




