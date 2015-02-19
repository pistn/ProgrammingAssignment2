## Put comments here that give an overall description of what your
## functions do

## This code creates two main functions 'makeCacheMatrix' and 'cacheSolve'.
## The first one takes a square matrix as an argument and returns a list
## that contains 4 functions that were defined inside this function.
## The second one takes as an argument the returned list from the first main
## function and returns the inverse matrix of the initial square matrix
## by using the functions inside the list and stores it in the cache.



## Write a short comment describing this function

## The first main function 'makeCacheMatrix' controls if the dimensions of the initial matrix
## are even (square matrix). Then 4 functions are defined 'set','get','setinv'
## and 'getinv'. The 'set' function can be used to store an initial matrix in the cache
## and to clear the cache if there is an inverse matrix already stored in it.
## The 'get' function returns the initial square matrix.
## The 'setinv' function stores in the cache the inverse matrix of the initial
## square matrix.
## The getinv function returns the inverse matrix that it is stored in the cache.
## Then the main function creates and returns the list of the above 4 functions.



makeCacheMatrix <- function(x = matrix()) {
  if( dim(x)[1]!=dim(x)[2] ){
    
    stop("argument must be a square matrix")
    
  }
  
  else{
    
    i<-NULL
    set<-function(y){
      x<<-y
      i<<-NULL
    }
    
    get<-function() x  
    setinv<-function(inv) i<<-inv
    getinv<-function() i
    list(set=set,get=get,
         setinv=setinv,
         getinv=getinv)
    
  }              

}


## Write a short comment describing this function

## The second main function 'cacheSolve' controls if there is already an inverse
## matrix in the cache by using the 'getinv' funtion, if there is it returns that matrix. 
## If there is not(and the initial matrix has not changed) it gets the initial square matrix by using the 'get' function.
## Then it coputes the inverse matrix of the initial one by using solve().
## Finally it stores the inverse matrix in the cache by using 'setinv' and returns
## the inverse matrix.


cacheSolve <- function(x, ...) {
  
  
    i<-x$getinv()
    
    if(!is.null(i)) {
      
      message("getting cached data")
      return(i)
    }
    
    mat<-x$get()
    i<-solve(mat,...)
    x$setinv(i)
    i
  
}
