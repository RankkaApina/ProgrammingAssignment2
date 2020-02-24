## These 2 functions are created to create a speacial matrix
## and to be able to cache its inverse, since computating it 
## every time can be costly timewise

## The function below creates a special matrix and sets its inverse
## to following steps:
    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the value of the inverse
    ## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) m <<- inverse
    getInv <- function() m
    list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The function below calculates the inverse of the function above.
## First the function checks if the inverse already exists, and if it does
## it uses the cached inverse. If it doesn't exist, the function proceeds
## to calculate the inverse and caching it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
