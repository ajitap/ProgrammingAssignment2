## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        setinvmatrix <- function(inverse) i  <<- inverse
        getinvmatrix  <- function() i
        list( set= set, 
              get = get, 
              setinvmatrix = setinvmatrix, 
              getinvmatrix = getinvmatrix)
}


## This function computes the inverse of the special "matrix" 
##returned by the function makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                i  <- x$getinvmatrix()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data  <- x$get()
        i  <- solve(data, ...)
        x$setinvmatrix (i)
        i
}
