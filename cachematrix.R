##In this example we introduce the <<- operator, which can be used to assign a 
## value to an object in an environment that is different from the current
## environment.  Below are two functions that are used to create a special "matrix"
## object that stores a numeric matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to:

## 1. Set the value of the Cache Matrix.
## 2. Get the value of the Cache Matrix.
## 3. Set the value of the Solve.
## 4. Get the value of the Solve.

makeCacheMatrix <- function(x = matrix()) {
        
        cm <- NULL
        set <- function(y){
                x <<- y
                cm <<- NULL
        }
        get <- function() x
        setcm <- function(matrix) cm <<- matrix
        getcm <- function() cm
        list(set = set, get = get,
             setcm = setcm,
             getcm = getcm)
}

## The cacheSolve is a function that computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. However, it first checks to see if the 
## inverse has already been calculated.  If so, it gets the inverse from the 
## cache and skips the computaiton.  Otherwise, uf the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        cm <- x$getcm()
        if(!is.null(cm)) {
                message("getting cached data")
                return(cm)
        }
        data <- x$get()
        cm <- solve(data, ...)
        x$setcm(cm)
        cm
}
