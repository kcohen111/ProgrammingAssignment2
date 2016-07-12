############################################################################
## makeCacheMatrix:                                                       ##
## This function creates a special "matrix" object that can cache its     ##
## inverse.                                                               ##
############################################################################

## list of function
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


############################################################################
## cacheSolve:                                                            ##              
## This function computes the inverse of the special "matrix" returned by ##
## makeCacheMatrix above.                                                 ##
## If the inverse has already been calculated (and the matrix has not     ##
## changed), then the cachesolve should retrieve the inverse from the     ##
## cache.                                                                 ##
############################################################################

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

#############################################################################
## testing:  
## 1. create matrix 
##    --> m <- matrix(rnorm(25), nrow = 5)  
## 2. call the function makeCacheMatrix to create a special "matrix" object 
##    --> smo <- makeCacheMatrix(m) 
## 3. get the matrix 
##    --> smo$get()
## 4. call cacheSolve to get the inverse 
##    --> cacheSolve(smo)
## 5. call cacheSolve again to confirm that we getting cached data
##    --> cacheSolve(smo)
############################################################################
