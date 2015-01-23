## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

      ## initialise 4 function to set/get both the matrix and its inverse in their own environment (function closure)
      ## getm will return NA if setm did not load a matrix first, otherwise will return the last setm matrix
      ## getinv will return NULL if neither setinv nor cacheSolve have been called before, otherwise return the inverse matrix
      
      invx <- NULL
      setm <- function(y) {
            x <<- y
            invx <<- NULL
      }
      getm <- function() x
      setinv <- function() invx <<- solve
      getinv <- function() invx
      list(setm = setm, getm = getm,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' by either finding its value or calculating it if not available
        ## then it is cached by default

            invx <- x$getinv()
            if(!is.null(invx)) {
                  message("getting cached inv matrix")
                  return(invx)
            }
            data <- x$getm()
            invx <- solve(data, ...)
            x$setinv(invx)
            invx
      
      
}

## how to test it
## create a matrix C
## create your functions MyMatrix                      MyMatrix <- makeCacheMatrix()
## initialise your MyMatrix with C                     MyMatrix$setm(c)
## check its value, should return c                    MyMatrix$getm()
## check NULL value for matrix inverse now             MyMatrix$getinv()
## call cacheSolve first time to calculate inv         cacheSolve(MyMatrix)
## call cacheSolve second time to red from cache       cacheSolve(MyMatrix)
## check its value                                     MyMatrix$getinv()



