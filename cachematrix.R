##Purpose is to minimze the time required to calculate the inverse of a matrix
#defined by the user. Time is reduced by caching existing values to the parent
#environment so that repetitive calculations are not performed

## The makeCacheMatrix function stores a matrix input by the user, clears any
#existing cache, and communicates with casheSolve() to send any newly
#calculated inverse values to cache

makeCacheMatrix <- function(x = matrix()) {

            #initialize cache for this function
            cache <- NULL
            
            #user defines matrix, existing cache is cleared
            set <- function(y) {
                  x <<- y
                  cache <<- NULL 
            }
            
            #returns user defined matrix, x
            get <- function() x
            
            #used to cache the inverse calculated in cacheSolve() by storing
            #the variable in the parent environment
            setinverse <- function(inverse) cache <<- inverse
           
            #function to retrieve the variable cache for use in cacheSolve()
            getinverse <- function() cache
            
            #returns a list of all functions defined in makeCacheMatrix()
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}

## The cacheSolve function retrieves whatever value is stored in the cache:i) If 
#the cache is empty (NULL), the function calculates the new inverse from the 
#data input by the user in makeCacheMatrix(), calculates the inverse using 
#solve(), stores this value to cache, and returns the inverse; ii) if the cache
#already contains a value, returns the string "getting cashed data", retrieves 
#cached value from the parent environment, and returns this value.

cacheSolve <- function(x, ...) {
      cache <- x$getinverse()
      
      #if cache contains a value, extract and return value
      if(!is.null(cache)) {
            message("getting cached data")
            return(cache)
      }
      
      #if cache is NULL, get user input data, calculate inverse, store inverse
      #in cache via makeCacheMatrix(), return value
      data <- x$get()
      cache <- solve(data, ...)
      x$setinverse(cache)
      cache
}
