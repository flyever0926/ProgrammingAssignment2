## makeCacheMatrix is to create a matrix object to cache its inverse.
## 

## This function returns a list of functions to set the matrix, get the matrix, set the inverse, 
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
         i<- NULL   ##Initalize the inverse
      
      ## set the matrix
      set=function(matrix) {
          x<<- matrix
          i<<- NULL
      }
      
      ## Get the matrix
      get<-function() x
      
      ## Set the inverse of the matrix
      setinverse<-function(inverse) i<<- inverse
      ## get the inverse
      getinverse=function() i
      
      ##return methods to a list
      list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)

}


## Calculate the inverse of a special matrix returned by
## MakeCacheMatrix. If the inverse is already calculated (and the
## matrix has not changed), then call "Cachesolve" to retrieve the inverse.


cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
      i<-x$getinverse()
         ## check if inverse has been calculated       
      if(!is.null(i)) {
          message("getting cached data")
          return(i)
      }
        ## if not, calculate the inverse
      x.data<- x$get()
      
      i=solve(x.data,...)
      
      x$setinverse(i)
      
      return(i)
}
