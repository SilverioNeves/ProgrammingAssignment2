## Silvério Neves - August 2018

## 2 functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
                ## validate if x it is a inversible matrix
                if (class(try(solve(x),silent=T))=="matrix"){
                        im <- NULL
                        set <- function(y) {
                                        ## validate if y it is a inversible matrix
                                        if (class(try(solve(y),silent=T))=="matrix"){
                                                x <<- y
                                                im <<- NULL        
                                        }
                                        else {
                                        print("object not a matrix or not a matrix invertible")
                                        }
                        }
                        get <- function() x
                        setinvm <- function(invm) im <<- invm
                        getinvm <- function() im
                        list(set = set, get = get,
                             setinvm = setinvm,
                             getinvm = getinvm)
                }
                else { print("object not a matrix or not a matrix invertible") }
        }


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
        im <- x$getinvm()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinvm(im)
        im
        ## Return a matrix that is the inverse of 'x'
}
