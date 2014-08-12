

####################################################
####################################################
# This R program calculates the inverse of a matrix
#
# Since this calculation can be expensive for a large
# matrix, this program caches the results. 
# 
# In case the inverse for a given matrix already has 
# been calculated, the answer will be given from the 
# cache instead if executing this expensive calculation again
#
# Example:
#
# May c be a 2 by 2 matrix:
#
# > c = rbind(c(1, -1/4), c(-1/4, 1))
# > c
#       [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# 
# > c2 = makeCacheMatrix(c)
#
# Then cacheSolve calculates the inverse matrix for 
# above given matrix c
#
# > cacheSolve(c2)
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# 
# Since the result already has been cached, the response
# will be taken from the cache (see message "getting 
# cached data") instead of recalculating it:
#
# > cacheSolve(c2)
# getting cached data
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
####################################################



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
      return(m)
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
   #
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
   # By definition, the matrix multiplied by its inverse matrix 
   # gives the identity matrix, i.e. a matrix with ones on the main 
   # diagonal (upper left to bottom right) and zeros at all other 
   # positions
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




