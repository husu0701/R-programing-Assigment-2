## makeCacheMatrix will create a matrix, inverse and cache it.
## cacheSolve will calculate the inversed matrix from makeCacheMatrix, and if the inverse has been calculated, then the cachesolve should retrive the inverse from cache.
makeCacheMatrix <-function(x=matrix()){ 
      inv <-NULL
      set <-function(y){
            x <<-y
            inv <<-NULL
      }
      get <-function(){x}
      setInverse <-function(inverse){inv <<- inverse}
      getInverse <-function(){inv}
      list(set = set,get = get, setIverse = setInverse, getInverse = getInverse)
} ##set and get the value of the matrix, and then set and get the value of inversed matrix. 


cacheSolve <- function(x,...){
      inv <- x$getInverse()
      if (!is.null(inv)){
            message("getting cached data")
            return (inv)
      } ##retrieve the inversed matrix if it has already been calculated.
      mat <- x$get()
      inv <-solve(mat,...)
      x$setInverse(inv)
      inv 
} ##return inversed matrix
