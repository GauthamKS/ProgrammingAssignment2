## Put comments here that give an overall description of what your
## functions do
#The functions makes a cche for the mean and also a funcion cacheSolve that solves the given input if
# the cache is empty
## Write a short comment describing this function
#The makeCacheMatrix function arguments = x return list of inverse functions
#It takes the input x and gives a list of functions to set, get the matrix and also to 
# set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <- NULL
  }
  get <-function()
  {
    x
  }
  setInv <-function(inverse)
  {
    inv <<-inverse
  }
  getInv <- function()
  {
    inv
  }
  list(set=set,get=get,setInv=setInv,getInv=getInv)

}


## Write a short comment describing this function
# The function takes in the list of cache functions and then evaluates the given matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <-x$getInv()
  if(!is.null(inv))
  {
    message("Cache not empty, returning cached value")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv
}

