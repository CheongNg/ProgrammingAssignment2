## Put comments here that give an overall description of what your
## functions do

## The function below calculates the inverse of a matrix and store it in cache

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <-function(y){
                x <<-y
                m <<- NULL
        }
        get<-function()x
        setinverse <- function(solve) m<<-solve
        getinverse<-function() m
        list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)        
}


## The function below checks for an inverse function from the cache if it has already been calculated
## Otherwise the function will calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
        
        m <-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
        
}
