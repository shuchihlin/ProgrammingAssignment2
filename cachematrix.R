## Functions makeCacheMatrix() and cacheSolve() are to allow 
## caching the matrix inverse computation results to avoid 
## re-computations 

# Function makeCacheMatrix(): 
# - take an argument of matrix "x" to return a special "matrix" which 
#   is really list containing 4 functions to:
#     set the value of the matrix of x
#     get the value of the matrix of x
#     set the value of the matrix inverse of x
#     get the value of the matrix inverse of x
makeCacheMatrix <- function(x = matrix()) {
    # initialize the matrix inverse
    m <- NULL  
    # set the matrix x and initialize the matrix inverse in containing env
    set <- function(new_x) {
        x <<- new_x
        m <<- NULL
    } 
    # get the current matrix x
    get <- function() 
    {
        return(x)
    }  
    # set the matrix inverse of x
    setinverse <- function(inverse) 
    {
        m <<- inverse
    }
    # get the matrix inverse of x
    getinverse <- function() 
    {
        return(m)
    }
    # return a list which contains the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Function cacheSolve(): 
# - take an argument of a special "matrix" created with makeCacheMatrix() 
#   to return its matrix inverse. However, the function first checks to see
#   if the matrix inverse has already been calculated. If so, it gets it 
#   from the cache and skips the computation. Otherwise, it calculates the 
#   matrix inverse from the current matrix and sets the matrix inverse in 
#   the cache via setinverse()
cacheSolve <- function(x, ...) {
    # get the cached "inverse of x"
    m <- x$getinverse()
    
    # return the cached "inverse of x" if already exists
    if(!is.null(m)) {
        return(m)
    }
    
    # cached "inverse of x" doesn't exist, get the current "x"
    data <- x$get()

    # calculate the inverse of "x"
    m <- solve(data, ...)
    # cache the result
    x$setinverse(m)
    
    # return the calculated "inverse"
    m
}
