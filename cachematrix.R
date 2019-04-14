
#function that create a special matrix with setters e getters for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    
    set <- function(y) {
      x <<- y
      mi <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        mi <<- inverse
    }
    getInverse <- function() {
        mi
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#function that retrives the inverse of the matrix from the cache, if i exists there, or calculate it, if not.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    mi <- x$getInverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    } else{
        message("not in cache")
    }
    
    data <- x$get()
    
    #the function solve() will return the inverse, if the matrix is invertible
    mi <- solve(data)
    x$setInverse(mi)
    mi
}
