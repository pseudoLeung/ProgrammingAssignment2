## Put comments here that give an overall description of what your
## functions do

## It may takes long time to calculate the inversed matrix of a matrix. If the contents of a matrix
## are not changing, it may makes sense to cahce the result of inversing matrix. Thus, when we need
## it again, it can be looked up in cache rather than recomputed.
## This function does as described above. When we inverse a matrix, the result will be cached. When
## we need it again, the cached result will be returned if no comment in matrix changed. If comments
## in matrix are changing, the result cached will be settled to NULL. Then inversed matrix will be
## recompute when needed.

## Write a short comment describing this function
## The first function, makeVector creates a special "vector", which is really a list containing a 
## function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    get <- function(){
        x
    }
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    getInverse <- function(){
        inv
    }
    setInverse <- function(inverse){
        inv <<- inverse
    }
    list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## This function calculates the inversed matrix of the matrix created with above function. 
## Firstly, it checks to see if the inversed has been already calculated.
## If so, it returns the cached result by `getInverse`. Otherwise, it calculates the inverse,
## and set the inversed cached by `setInverse` function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if( !is.null(inv)){
            message("getting cached inversed matrix")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
