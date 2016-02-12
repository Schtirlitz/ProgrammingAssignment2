## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
          x <<- y
          m <<- NULL
  } # end function "Set"
  
  get <- function() x
  
  set_inverse_matrix <- function(solve) m <<- solve
  
  get_inverse_matrix <- function() m
  
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
} # end function "makeCacheMatrix"


## Return a matrix that is the inverse of matrix 'x'

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse_matrix() # get inverse matrix from cache
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }# end if
  
  data <- x$get() # get matrix 'x'
  m <- solve(data, ...) # calculate inverse of matrix 'x'
  x$set_inverse_matrix(m)
  m
}  # end function "cacheSolve"
