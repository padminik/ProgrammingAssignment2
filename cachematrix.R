## These functions cache the inverse of a matrix

## makeCacheMatrix makes a special matrix to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
set<- function(y){
        x<<-y
        inv<<- NULL
}
get<- function()x
setinv<- function(inv) i<<-inv
getinv<-function()i
list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve checks if the inverse exists and solves for the inverse if not
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
