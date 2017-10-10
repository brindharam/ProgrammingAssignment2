## R-Programming (Course 2), Week 3 Programming Assignment

### Problem statement: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will 
## not discuss here). Your assignment is to write a pair of functions that 
## cache the inverse of a matrix.


## Creates a special 'matrix' object
## For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(z = matrix()) {
        invmtrx <- NULL
        
        # set the value of matrix x
        set <- function(mtrx) {
                z <<- mtrx
        }
        
        # retrieve the value of matrix x
        get <- function() z
        
        # set the value of inverse matrix
        setInverseMatrix <- function(y) {
                invmtrx <<- y
        }
        
        # get the value of inverse matrix
        getInverseMatrix <- function() invmtrx
        
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## Computes the inverse of the special 'matrix' returned by makeCacheMatrix
## function. If the inverse has already been calculated (and the matrix has not
## changed), then this method will retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- z$getInverseMatrix()
        if (!is.null(m)) {
                print("getting cached data")
                return(m)
        }
        data <- z$get()
        print(data)
        m <- solve(data)
        z$setInverseMatrix(m)
        m
}
