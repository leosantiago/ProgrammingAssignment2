## makeCacheMatrix creates a list with 4 functions
## get  gets the value of the matrix
## set  sets the value of the matrix
## getinv   gets the value of the inverse of the matrix
## setinv   sets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inv<- NULL
   set <- function(y){
      x<<-y
      inv<<-NULL
   }
   get <- function() { x }
   setinv <- function(inverse) { inv<<-inverse }
   getinv <- function() {inv}
   list(set=set, get = get,setinv = setinv,getinv=getinv)
}


## cacheSolve calculates the inverse of a matrix and caches it
## if this inverse has already been cached it gets the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv<- x$getinv()
   if(!is.null(inv)){
      message("getting cached data")
      return(inv)
   }
   data <- x$get()
   inv<- solve(data)
   x$setinv(inv)
   inv
}
