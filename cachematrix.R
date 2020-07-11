## Put comments here that give an overall description of what your
## functions do

## Caching the Mean of a Vector

makeCacheMatrix <- function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function(){x}
  setinverse <- function(inverse) {inv<<-inverse}
  getinverse <- function() {inv}
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Caching the Inverse of a Matrix
cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
}
