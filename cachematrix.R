## Put comments here that give an overall description of what your
## functions do




## Write a short comment describing this function

##makeCacheMatrix function takes in a matrix argument x and binds four related functions as a list. 
##It calls another function cacheSolve to calculate the inverse of the matrix as well as to cache the results for future access.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  
  setinverse<-function(x) inverse<<-solve(x)
  
  
  getinverse <- function() {print(environment());inverse}
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##Calculates the inverse of a matrix using the solve in-built function

##Returns inverse from cache if inverse has been calculated already

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()  
  if(!is.null(inverse)){ 
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
