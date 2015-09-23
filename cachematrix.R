## makeCacheMatrix creates a list of functions that store a matrix
## The calling function is then able to set the matrix and get its values
##calling "set" nullifies the "inverse calculated" flag


## call this function to initialize the code and the data
## example:  b <- makeCacheMatrix(matrix(c(1,2,3,5),2,2))
##
##c.bahr code based on the class example


makeCacheMatrix <- function(x = matrix()) {

  x_inverse <- NULL
  set <- function(y) {
    
    ## asssign to "global" enviroment
    
    x<<- y
    x_inverse <<- NULL
  }
  
  ## return set value
  
  get <- function() x
  
  setinverse <- function(x_inv) x_inverse <<- x_inv
  
  getinverse <- function() x_inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
    
  }
  


## cachesolve calculates the matrix inverse if it discovers
## that the matrix has been changed (and initially)
## call this code to get the answer
## example:  cacheSolve(b)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinverse <- x$getinverse()
  if(!is.null(xinverse)) {
    message("getting cached inverse")
    return (xinverse)
  }
  datamatrix <- x$get()
  xinverse <- solve(datamatrix)
  x$setinverse(xinverse)
  xinverse
}
