## Put comments here that give an overall description of what your
## functions do

## This function creates a special object "matrix" that can cache it's on inverse

makeCacheMatrix <- function(x = matrix()) {
        a
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of "matrix" cached by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
        
}
