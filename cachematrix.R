## Put comments here that give an overall description of what your
## functions do

##Created two functions. The first (makeCacheMatrix) creates dummy variables and set and get functions
  #for a square matrix that we want to invert using Solve. The recond (cacheSolve) takes the dummy 
  #variables from the first function, and either computes the inverse of the given matrix or retrieves
  #the already inverted matrix from it's cache.

## Write a short comment describing this function
#Sets up dummy/formal varibles and get/set functions for a square matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
      set <- function(y){
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setInvertedMatrix <- function(InvertedMatrix) m <<- InvertedMatrix
    getInvertedMatrix <- function() m
    list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}


## Write a short comment describing this function
## Computes inverse of matrix or retrieves cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInvertedMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvertedMatrix(m)
    m
  
}

aMatrix <- makeCacheMatrix(matrix(1:4, 2,2))
aMatrix$get()
aMatrix$set(matrix(1:4, 2,2))
cacheSolve(aMatrix)
