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
    mi <- solve(data)
    x$setInverse(mi)
    mi
}

mat <- read.table("matrix", header = FALSE,sep=" ")
matC <- makeCacheMatrix(mat)

cacheSolve(matC)
