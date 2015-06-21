## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#-set the value of the matrix
#-get the value of the matrix
#-set the value of the inverse
#-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
          x <<- y
          i <<- NULL
    }
    
## <<- assigns a value to an object, in this case the inverse, in an environment different from the current environment which in this case is the cache
    
    get <- function() x
    setmatrix <- function(solve) i <<- solve
    getmatrix <- function() i
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## The above part sets and gets the value of the mean


## The following function calculates the mean of the special "matrix" created with the above function. It first checks to 
## see if the mean has already been calculated. If so the computation is skipped and the mean is retrieved 
# from the cache created with the newly introduced <<- operator

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    i <- x$getmatrix()
    if(!is.null(i)) {
        #If the mean is already calculated skip the computation and retrieve it from the cache and inform the user with the message "getting cached data"
            message("getting cached data")
            return(i)
}

        #This step occurs if the mean has not yet been calculated. It calculated the mean and then sets the value of the mean in the cache for future use
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setmatrix(i)
    i
}

