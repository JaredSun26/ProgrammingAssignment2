## The functions are primarily created based on the example code.

## This defines a list of functions to 
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  #function to set the value of the matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  #function to get the value of the matrix
  get<-function() x
  
  #function to set the inverse
  setInv<-function(inverse) inv<<-inverse
  
  #function to get the inverse
  getInv<-function() inv
  
  list(set = set, get =get, setInv=setInv, getInv=getInv)
}


## function to retrieve or calculate the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

#test example:
# > b<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2))
# > cacheSolve(b)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(b)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5






