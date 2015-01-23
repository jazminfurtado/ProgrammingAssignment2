#This code outputs the inverse of a matrix

#makeMatrix creates a list that contains the values of setting and getting the 
##matrix and inverse
makeCacheMatrix<-function(x=matrix()){
  m <- NULL
  #set, get, setinverse, getinverse are functions for the inputted matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x    
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve uses the list and inverts the matrix,'x' 
##if it has not already been inverted
cacheSolve<-function(x, ...){
  m <- x$getinverse()
  #if there is a value in m, return it; if not, find inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}






make.power<- function(n){
  pow<-function(x){
    x^n
  }
  pow
}

cube<-make.power(3)
square<-make.power(2)
cube(3)
square(3)