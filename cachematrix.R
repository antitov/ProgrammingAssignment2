## Function makes special "matrix" object. There is a matrix-type check at the beginning.
## If an argument is not a matrix function will return an error.
## MakeCacheMatris is a list of functions that 
## 1. get value of the matrix
## 2. set value of the matrix
## 3. get the inversed matrix
## 4. set the inversed matrix
## To be noted: there is no check if matrix is singular (therefore noninvertable). 
## So matrix should be invertable to use cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        if(class(x)!="matrix") {
                message("Error: Please feed the matrix to the function." 
                        "You were trying to feed a ", class(x), " instead of a matrix")
                return()
        }
        set <- function(y) {
                if(class(y)!="matrix") {
                        message("Error: Please feed the matrix to the function.",
                        "You were trying to feed a ... ", class(y), " instead of a matrix")
                        return()
                }
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() calculates the inversed matrix of a special "matrix" created by makeCacheMatrix()
## If matrix is noninvertable then cacheSolve() will return an error.
## Also matrix inversion can be appied only to square matrixes.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("Getting cached inverted matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv

}
