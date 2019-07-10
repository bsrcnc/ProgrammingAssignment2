##The inverse of a matrix can be found with  makeCacheMatrix function by doing this we can save time instead of calculating the 
## inverse of same matrix. 

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) 
  inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)


}



cacheSolve <- function(x, ...) {
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("it's cached result!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        
        }
