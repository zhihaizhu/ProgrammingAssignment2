##  Caching the Inverse of a Matrix
##  Matrix inversion is usually a costly computation 
##  and there may be some benefit to caching
##  the inverse of a matrix rather than compute it repeatedly

##  The following function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
                 Inv <- NULL
                 set <- function(y){
                   x <<- y
                   Inv <<- NULL
}
                  get <- function() x
                  setInv <- function(Inverse) Inv <<- Inverse
                   getInv <- function() Inv
                 list(set = set, get=get, setInv = setInv, getInv=getInv)
}
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed) 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
                Inv <- x$getInv()
                if(!is.null(Inv)){
                 message("getting the cashed data")
                 return(Inv)
  }
                data <- x$get()
                Inv <- solve(data, ...)
                 x$setInv(Inv)
                 Inv
}
}
