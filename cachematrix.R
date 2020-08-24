## makeCacheMatrix creates a special “matrix” object that can cache its inverse
## cacheSolve computes the inverse of the matrix returned by the other function
## if the inverse has been calculated and the matrix hasn´t changed,
## it retrieves the inverse from the cache

## the 1st function returns a list containing functions to 
## set the matrix
## get the matrix
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set = function(y){
                x <<- y
                inv <<- NULL
                
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
 
}


## computes the inverse of the matrix returned by the other function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if (!is.null(inv)){
                #get it from the cache instead of calculating it again
                message("getting cached data")
                return(inv)
                
        }
        #if the condition is not met, the inverse will be calculated
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        return(inv)
}
