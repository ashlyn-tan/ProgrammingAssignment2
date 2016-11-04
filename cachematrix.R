## The functions below compute the inverse of a matrix with caching rather than computing it repeatedly

## makeCacheMatrix creates a special 'matrix' object that can cache its inverse
## the special 'matrix' returned will be used for the cacheSolve function

makeCacheMatrix=function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(inv) m<<-inv
  getinv<- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve retrieve the inverse of the matrix from the cache if the inverse is already calculated in the first function. If not, it will computes the inverse of the matrix again.

cacheSolve=function(x, ...){
  m<- x$getinv()
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
}