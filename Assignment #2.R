#Honestly lost with this assignment
#I'd appreciate any feedback that'll help me get started
#I used the partial code from the instructions
#I assumed that's okay
makeCacheMatrix <- function(x = matrix()){
  get <- function(x)
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function(z) 
    inverse <- NULL
  set <- function(y) 
    x <<- y
    inverse <<- NULL
  list(set = set, get = get,
       invSet = invSet,
       invGet = invGet) #Error in this: "object 'invSet' not found"
}

cacheSolve <- function(x, ...) {
  z <- x$getinv()#Error: "operator is invalid for atomic vectors"
  if(!is.null(z)) {
    message("receiving cached data")
    return(z)#This function checks if inv and sends message if not inv
  }
  data <- x$get()
  z <- inverse(data, ...)
  x$invSet(z)
  z
}
makeCacheMatrix(matrix(1:10,3,9))
cacheSolve(3)
