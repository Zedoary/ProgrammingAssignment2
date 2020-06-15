## Put comments here that give an overall description of what your
## functions do

##First function makeCacheMatrix creates a special matrix object 
##that can cache its inverse.
##Second function cacheSolve solves the inverse of the the special
##matrix object if it did not calculate it before and is not cached.


## Write a short comment describing this function
##set i as null for the matrix
##assign x and inverse to the parent environment
##If there is an value for inverse then set the value to i
##in the parent environment
##Create a list with the values for set, get, setInverse, and getInverse


makeCacheMatrix <-function(x=matrix()) {
  i <- NULL
  set <-function(y) {
    x<<-y
    inverse<<-null
  }
  
  get <-function() x
  setInverse <-function(inverse) i <<-inverse
  getInverse <-function() i
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Write a short comment describing this function
## if inverse is in the cache, return a message and the inverse matrix
## if inverse is not there for the matrix then solve for the inverse
## and return the inverse

cacheSolve<-function(x, ...) {
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data, ...)
  x$setInverse(i)
  i
}
## Return a matrix that is the inverse of 'x'