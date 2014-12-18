
## makeCacheMatrix creates a special "vector" that stores a matrix and
## provides methods to to get & set the value of a matrix and get & set 
## the reverse of the stored matrix.


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  
  reverse = NULL

  set <- function(y) {
    y <<- x
    reverse <<- NULL 
  }
  get <- function() x
  setreverse <- function(solve) reverse <<- solve
  getreverse <- function() reverse
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
}


## cacheSolve returns the reverse of matrix created using makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by 
  ## makeCacheMatrix above. If the inverse has already been calculated (and 
  ## the matrix has not changed), then the cachesolve should retrieve the 
  ## inverse from the cache.
  
  r <- x$getreverse()
  
  ## check if the reverse is chached alredy.
  ## if so, return the chached reverse without calculating
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  
  ## reverse is not chached, have to calculate. 
  data <- x$get()
  r <- solve(data, ...)
  ## cache the reverse
  x$setreverse(r)
  ## and return the calculated reverse
  r
}
#
# Usage
# 
# > x <- makeCacheMatrix(matrix(sample(9), 3,3))
# > x$get()
#      [,1] [,2] [,3]
# [1,]    4    5    6
# [2,]    7    9    3
# [3,]    2    1    8
# > cacheSolve(x)
#        [,1]  [,2]   [,3]
# [1,] -1.725  0.85  0.975
# [2,]  1.250 -0.50 -0.750
# [3,]  0.275 -0.15 -0.025
# > 
# > y <- cacheSolve(x)
# getting cached data
# > z <- x$get()
# > y %*% z
#              [,1]          [,2]          [,3]
# [1,] 1.000000e+00 -1.110223e-16 -8.881784e-16
# [2,] 6.661338e-16  1.000000e+00  8.881784e-16
# [3,] 2.081668e-17  1.040834e-17  1.000000e+00
# > 
# > 
# > x <- makeCacheMatrix(matrix(sample(16), 4,4))
# > cacheSolve(x)
#              [,1]        [,2]        [,3]         [,4]
# [1,]  0.007438795  0.06581921  0.01807910 -0.120244821
# [2,] -0.060075330  0.08870056 -0.07005650  0.059698682
# [3,] -0.083804143 -0.07062147  0.06214689  0.164783427
# [4,]  0.098305085 -0.05423729  0.02372881 -0.006779661
# > 
# > y <- cacheSolve(x)
# getting cached data
# > z <- x$get()
# > 
# > y %*% z
#               [,1]          [,2]          [,3]          [,4]
# [1,]  1.000000e+00  1.110223e-16  0.000000e+00  2.220446e-16
# [2,] -3.053113e-16  1.000000e+00 -1.110223e-16 -1.110223e-16
# [3,] -1.110223e-16 -2.220446e-16  1.000000e+00  0.000000e+00
# [4,] -5.030698e-17  2.081668e-17  1.387779e-17  1.000000e+00
# > 
