## Author: DAssignments
## Github: github.com/DAssignments

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function, makeCacheMatrix creates a special "matrix", which is really a
# list of functions that allow us to modify and cache the matrix and its inverse
# These are the functions of the list returned:
#  - set(·) stores the value of the matrix using the superassignment
#           operator <<- in the environment of makeCacheMatrix(·)
#  - get(·) returns the value of the matrix stored
#           in the environment of makeCacheMatrix(·) 
#  - setinvm(·) stores the value of the inverse of the matrix using the super-
#               assignment operator <<- in the environment of makeCacheMatrix(·)
#  - getinvm(·) returns the value of the inverse of the matrix stored
#               in the environment of makeCacheMatrix(·)

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinvm <- function(invmx) invm <<- invmx
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}


## Write a short comment describing this function

# This function, cacheSolve(·) computes and caches the inverse of the matrix
# stored in the environment of makeCacheMatrix(·) if it has not already been
# calculated, otherwise this function only returns the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvm(invm)
    invm
}

# Example of usage:
# D_m <- 2000
# my_m <- matrix(rnorm(D_m * D_m), D_m, D_m)
# my_m <- my_m %*% t(my_m) # to ensure that my_m is invertible
# my_inv_aux <- round(solve(my_m), 8)
# 
# l_makeCacheMatrix <- makeCacheMatrix(my_m)
# t_ini <- proc.time()[3]
# my_invm <- round(cacheSolve(l_makeCacheMatrix), 8)
# t_fin <- proc.time()[3]
# print (paste(t_fin - t_ini, "s", sep=""))
# identical(my_invm, my_inv_aux)
# 
# t_ini2 <- proc.time()[3]
# my_invm <- round(cacheSolve(l_makeCacheMatrix), 8)
# t_fin2 <- proc.time()[3]
# print (paste(t_fin2 - t_ini2, "s", sep=""))
# identical(my_invm, my_inv_aux)