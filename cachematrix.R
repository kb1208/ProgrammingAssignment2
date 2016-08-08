## This function is created as a shortcut to computing the ivnerse of a matrix 
## repeatedly. Instead once the inverse is computed this function instead of computing
## it again it uses the previous computation if nothing has changed

## Here I have created a function which includes a list of functions designed to 
## several different things to ultimately get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()){
  s<-NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function(){
   x
  }
 
  setInverse <- function(solve){
  s <<- solve(x)
  }
  
  getInverse <- function(){
    s
  }
  
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The function below calculates the inverse of a function. If the inverse is 
## calcualted already then this function will get the matrix from the cache. 
## If it is the first time the inverse it is being calculated then the function
## uses the original matrix to find its inverse.

cacheSolve <- function(x, ...) {
  
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s
        ## Return a matrix that is the inverse of 'x'
} 

