## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
            matrix_solved <- NULL   ## matrix_solved is created in memory, holds the solved value and initiates at a NULL value
            set <- function(y) {
              x <<- y              ## y is assigned x using <<- since the set() is called in a different environment from the parent environment. 
              matrix_solved <<- NULL ## matrix_solved resets to NULL everytime the set() is called introducing new values
            }
            get <- function() x
            setsolve <- function(solve) matrix_solved <<- solve ## solves the matrix and sets its value to matrix_solved
            getsolve <- function() matrix_solved                ## retrieves the most current values of matrix_solved
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_solved <- x$getsolve()
  if(!is.null(matrix_solved)) {
    message("getting cached data")
    return(matrix_solved)          ##checks for the current values of matrix_solved, retrieves cached valies hence the "message"
  }
  data <- x$get()    ## retrieves set values of x, performs the solve() and cache the value, then prints to console
  matrix_solved <- solve(data, ...)
  x$setsolve(matrix_solved)
  matrix_solved
}
