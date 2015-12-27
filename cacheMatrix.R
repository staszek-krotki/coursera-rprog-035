# This is a function that returns a special cache matrix object which is a list of methods to operate on it:
#   set() - sets the matrix
#   get() - gets the matrix
#   setInversedMatrix() - sets the inversed matrix
#   getInversedMatrix() - gets the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    inversed_matrix <- NULL
    
    set <- function(y) {
        x <<- y
        inversed_matrix <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInversedMatrix <- function(m) {
        inversed_matrix <<- m
    } 
    
    getInversedMatrix <- function() {
        inversed_matrix
    }
    
    list(set = set, get = get, setInversedMatrix = setInversedMatrix, getInversedMatrix = getInversedMatrix)
}

# This function solves a matrix. It can only operate on the object returned by makeCacheMatrix
# if the matrix has already been solved then it is returned form the cache

cacheSolve <- function(x, ...) {
    m <- x$getInversedMatrix()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInversedMatrix(m)
    
    m
}