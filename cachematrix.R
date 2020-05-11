## In this example we introduce the <<- operator which can be used to assign a value
##to an object in an environment that is different from the current environment.

## This function creates a special object "matrix" that can cache it's on inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL                                          ## Global environment
        set<-function(y){                                ## sets the value of the matrix
                x<<-y
                i<<-NULL
        }
        get<-function() x                                ##Gets the value of Matrix
        setinverse<-function(inverse) i<<-inverse        ## Sets the inverse of matrix
        getinverse<-function() i                         ##Gets the inverse of Matrix
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of "matrix" cached by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)) {                                ##checks whether inverse is already calculated
                message("getting cached data")           ##If so, we get the inverse of matrix and computation is skipped
                return(i)
        }
        data<-x$get()                                     
        i<-solve(data,...)                               ##Otherwise, inverse is calculated
        x$setinverse(i)                                  ##and it's value is set in the cache via setinverse function
        i
        
}
