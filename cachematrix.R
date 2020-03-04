## Write a short comment describing this function

## The makeCacheMatrix function gets matrix x, takes it as an input
## And sets the matrix value and gets the matrix value
## Sets the matrix inverse and gets the matrix inverse
## object x can cache its own object (inverse)

makeCacheMatrix <- function(x = matrix()) {               ## x is a square invertible matrix
  inv = NULL
  set = function(y) {                                     ## "set" = set value of matrix
    x <<- y                                               ## "<<-" = assigns a value to an object x in a different environment
    inv <<- NULL
  }
  get = function() x                                      ## "get" = get value of matrix
  setinv = function(inverse) inv <<- inverse              ## "setinv" = set the inverse of the matrix
  getinv = function() inv                                 ## "getinv" = get the inverse of the matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)    ## "set=set, get=get, setinv=setinv, getinv=getinv" is the inputted list for the function "cacheSolve"
}


## Write a short comment describing this function

## The "makeCacheMatrix" output is taken as an input by the "cacheSolve" function
## Then "cacheSolve" checks whether or not the inverse matrix from "makeCacheMatrix" contains any value
## If there is a value, the message "getting cached data" is returned as well as the cached object
## If there is no value, the original matrix data is retrieved, the invertible matrix is set w/ "solve"

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## Get the invertible matrix value from "makeCacheMatrix"
  
  inv <- x$getinv()
  if (!is.null(inv)) {                                    ## If inverse of matrix is not NULL
    message("getting cached data")                        ## Then type message:  "getting cached data"
    return(inv)                                           ## Returns the invertible matrix
  }
  
  ## If inverse of matrix is NULL...
  
  mat.data = x$get()                                      ## Gets the matrix data
  inv = solve(mat.data, ...)                              ## "Solve" inverts the matrix
  x$setinv(inv)                                           ## Sets the invertible matrix
  return(inv)                                             ## Returns the invertible matrix 
  
}