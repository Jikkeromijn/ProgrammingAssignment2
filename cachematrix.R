## In this R script, two functions are defined
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix is a function that makes an empty vector called x
makeCacheMatrix <- function(x = matrix()) {

  ## cacheSolve will create an inversion of the inputted matrix called m. makeCacheMatrix sets m to NULL as a default
  ## if m equals NULL, the matrix has not been inverted before and no cached inversion is available
  m <- NULL

  ## the function called set is a function of y
  set <- function(y) {
    ## the set function caches y as x
    x <<- y
    ## and it resets the value of m to NULL 
    ## otherwise cacheSolve will wrongfully assume that the matrix has already been inverted
    m <<- NULL
  }
  
  ## the original matrix stored in x is assigned to the function called get
  get <- function() x
  ## an anonymous function inverts the matrix and calls it m, and the function called setinverse can call this function
  setinverse <- function(inverse) m <<- inverse
  ## the output of m is assigned to the function called getinverse
  getinverse <- function() m
  ## the four set and get functions are put togetger in a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calls functions that were stored in the special matrix returned by makeCacheMatrix
  cacheSolve <- function(x, ...) {
    
    ## it retrieves the output from the inverted matrix and  assigns this output to m 
    m <- x$getinverse()
    ## IF cacheSolve has run on this same matrix before, it returns the inverted matrix from cache and tells us that it did so
    if(!is.null(m)) {  
      message("getting cached data")  
      return(m)
    }
    
    ## but ELSE, if m equals NULL, there is no cached inverted matrix
    ## and a new function called data retrieves the output from the get variable in the list
    data <- x$get()
    ## calculated the inverted matrix by performing solve on the data function and assigning this to m
    m <- solve(data, ...)
    ## storing the inverted matrix to the setinverse function and returning the inverted matrix
    x$setinverse(m)
    return(m)

}
