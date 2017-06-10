## Objective:  Create an invertible matrix
##             Check cache for the inverse of the matrix.
##                  If it exists, retrieve it from cache.
##                  If it does not exist, calculate the inverse and cache it.
##              Assume the matrix supplied is always invertible

## makeCacheMatrix creates an invertible matrix

makeCacheMatrix <- function(x=matrix()) {
        m <-NULL
        set <- function(y) {
                x <<- y
                m<<-NULL
        }
        get<- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        list(set=set,get=get,
             setsolve=setsolve, getsolve=getsolve)
}
## cacheSolve checks cache for the inverse of the arg matrix created in makeCacheMatrix.
## If cache is not null, return the value (the inverse of the matrix)
## If cache is null, calculate the inverse of the matrix, store it in cache, and return the value

cacheSolve <- function(x, ...) {
                m <- x$getsolve()        
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
       
}
## test case:  m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2 
## mymatrix<-makeCacheMatrix(m1)
## cacheSolve(mymatrix)
## should return exactly the matrix n1
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## calling cacheSolve again should retrieve (not recalculate) n1
## n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
## mymatrix$set(n2) sets x as n2 in the makeCacheMatrix object and resets cache to NULL to trigger cacheSolve again
## cacheSolve(mymatrix)