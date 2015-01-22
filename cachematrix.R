## A pair of functions that cache the inverse of a matrix.

## Input : Pass an invertible square matrix to the "makeCacheMatrix" function
##         and store the named list returned by the function to an object(ex. mat).

## Example : >myMatrix <- matrix(c(4,3,3,2),2,2)
##                    [,1] [,2]
##              [1,]    4    3
##              [2,]    3    2
##           >mat <- makeCacheMatrix(myMatrix)

## When makeCacheMatrix is first called, it creates a unique environment
## (basically a namespace for variables). This environment persists (stays
## in memory) because there is a pointer to the instantiation of the function
## (mymyMatrix in the example above). As long as that pointer exists, the
## environment stays in memory.

## Now call : >cacheSolve(mat)
        
## Output :       
##          [,1] [,2]
##    [1,]   -2    3
##    [2,]    3   -4
                      
## When we call cacheSolve again, then it retrieves the inverse from the
## cache since the inverse was already calculated (and the matrix has
## not changed)

## Again, call : >cacheSolve(mat)

## Output :       
##    getting cached data
##          [,1] [,2]
##    [1,]   -2    3
##    [2,]    3   -4


## The function "makeCacheMatrix" creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
