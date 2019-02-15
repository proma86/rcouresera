## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix function creates a special "matrix", which is a list containing a function to set the value of the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse. 



makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(y) {
                x <<- y
                matrix_inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) matrix_inv <<- solve
        get_inverse <- function() matrix_inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}



## The cacheSolve function calculates the inverse of the special "matrix" created with the makeCacheMatrix function provided it does not exist in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inv <- x$get_inverse()
        if(!is.null(matrix_inv)) {
                message("getting cached data")
                return(matrix_inv)
        }
        data <- x$get()
        matrix_inv <- solve(data, ...)
        x$set_inverse(matrix_inv)
        matrix_inv
}

mat<-matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
m1<-makeCacheMatrix(mat)
cacheSolve(m1)
