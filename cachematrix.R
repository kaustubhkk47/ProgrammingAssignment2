## The two functions are used to create a special object
## that stores a matrix and caches its inverse.

## Function does the following and creates a list
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Set i, the inverse as a null matrix
    
    i <- matrix()    
    set <- function(y) {
        x <<- y
        i <<- matrix()
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to get inverse of matrix object

cacheSolve <- function(x, ...) {
    
    ## Check if inverse exists and if it does, return it
    
    
    data <- x$get()
    i <- x$getinverse()

    if(!is.na(i[1,1]) || dim(i)[1] != 1 || dim(i)[2] != 1) {
        message("getting cached data")
        return(i)
    }
    ## If inverse doesn't exist, solve and cache solution
    
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
