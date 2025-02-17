## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function()x
        setinv <- function(inverse)inv<<-inverse
        getinv <- function(){
                inver <- ginv(x)
                inver%*%x
        }
        list(set = set, get=get,
             setinv = setinv,
             getinv = getinv)
}



cacheSolve <- function(x, ...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}

pmatrix <- makeCacheMatrix(matrix(1:8, nrow=2, ncol=4))
pmatrix$get()
pmatrix$getinv()
cacheSolve(pmatrix)
