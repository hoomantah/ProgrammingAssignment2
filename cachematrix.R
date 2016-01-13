## Assignment for the week 3 of the R Programming course
## makeCacheMatrix creates an inverse-caching matrix
## cacheSolve tries to inverse the inverse-caching matrix
## or returns the cached version of it.

## Construct a new type of matrix, storing its inverse
## into its cache when calculated. Also, retrieves the
## cached inverse, if not changed, and if calculated.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Inverse of the matrix of type makeCacheMatrix would
## be returned if possible, and if the det(x) is non zero.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("The matrix has its inverse done before; retreiving cached data...")
                return(m)
        }
        data <- x$get()
        if(det(data)!=0) {
                m<-solve(data,...)
                x$setinverse(m)
                return(m)
        } else {
                stop("The determinant of the matrix is 0, so it is not inversible")
        }
}

