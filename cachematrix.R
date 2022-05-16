#John Hopkins R Programming Assignment 2 Week 3
#I'd appreciate any feedback as I am still somewhat confused

#This function is for creating matrices (regular and inv) and cache them

makeCacheMatrix <- function(x = matrix()){
    inverse <<- NULL
    set <- function(y){ 
      x <<- y
      inverse <- NULL
    }
    get <- function()x
    invSet <- function(inv) inverse <<- inv
    invGet <- function()inv
    list(set = set, get = get,
       invSet = invSet,
       invGet = invGet) 
}

#The function is for caching the inv of the makeCacheMatrix and 
#return matrix of inv

cacheSolve <- function(x, ...) {
  z <- x[[invGet()]]
  if(!is.null(z)) {
    message("receiving cached data")
    return(z)
  }
  data <- x[[get()]]
  z <- inverse(data, ...)
  x[[invSet(z)]]
  z
}

#test code

c <- makeCacheMatrix(matrix(1:10,3,9))
cacheSolve(c)

