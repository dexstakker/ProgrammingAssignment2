## This file defines a function, makeCachingMatrix(), that creates a matix that is 
## capable of caching the results of a calculation of its inverse 
## so that it can later be retrieved without recalculting 

## In addition, it defines another function, cachingSolve(), that 
## calculates the inverse of a matrirx created by makeCachingMatrix
## if it had not already been calculated for the current value of 
## its matrix. If it had, it returns the previously calculated 
## inverse

## Creates a matrix that is automatically capable of 
## caching its calculated inverse
makeCachingMatrix <- function(x = matrix()) {
  if (nrow(x) != ncol(x) || nrow(x) <= 0) {
    stop("invalid matrix size: shoud be square matrix of non-zero size")
  }

  # m is the variable for storing the inverse, defined
  # inside of this function, because it is only pertinent
  # to whatever variable instance you assign this function to
  m <- NULL
  
  # set() sets the overall matrix value, it assigns parameter matrix "y"
  # to the "x" one level up (inside of makeCachingMatrix), and
  # clears the "m" one-level up of it's cached value because 
  # we changed the base matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get() - a function that returns possibly modified makeCachingMatrix
  # parameter matrix "x"
  get <- function() x

  # setsolve(solve) a function that takes a matrix parameter
  # for the invertedMatrix and saves it in the "m" defined 1 level)
  # up from this function: inside makeCachingMatrix
  setsolve <- function(solvedMatrix) m <<- solvedMatrix
  
  # getsolve  function returns the inverted matrix stored in 
  # "m" one level up in makeCachingMatrix function
  getsolve <- function() m 

  # list of four basic function defined above, names
  # so we can use matrix$funcName notation
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cachingSolve(x) is a function that takes new cachingMatrix as input and 
## can return the cachced value if available of calculate the
## new one if not

cachingSolve <- function(x, ...) {
  print(x)
  ## Return the matrix that is cached currently as inverse of 'x'
  invMat <- x$getsolve()
  
  if (!is.null(invMat)) {
    # if it's not null, then we successfully 
    # grabbed cached value, so return it
    message("getting cached data")
    return(invMat)
  }
  
  # Otherwise, we have to make sure....
  
  # ...we grab the original matrix and...
  data <- x$get()
  #...calculate the inverse of it...
  invMat <- solve(data, ...)
  #...and store it back in x as the cached inverse...
  x$setsolve(invMat)
  #...and return the inverse from this function....
  invMat
}

