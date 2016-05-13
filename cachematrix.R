## This group of funtions designed to cache value of an inverted matrix. 
## They provide closure functions that set and get a matrix,  
## or set and get matrix inverse. 

## @name: makeCacheMatrix
## This function takes invertible matrix as its parameter
## @param: x invertible matix
## provides number of closure functions to set or get a matix.
## It initializes inverse matrix variable as a global variable using 
## super assignment operator

## @return: List of set and get functions that set/get a matrix or
##          set/get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
       x <<- y
       m <<- NULL
    }
    
  get <- function() x
  
  setInverse <- function(inv_matrix) m <<- inv_matrix
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## @name: cacheSolve
## This function takes a list of functions as its input parameters
## It checks inverse of a matrix variable is not NULL.
## If the variable is not NULL it returns the inverse of the matrix
## Otherwise it inverses the matrix and returns the inverted matrix

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
       m <- x$getInverse()
       if(!is.null(m)) {
          message("getting cached data")
          return(m)
       }
      data <- x$get()
      
      ## Set inverse of the matix 'x' and return it
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
