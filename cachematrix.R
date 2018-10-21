## Coursera R Programming Class Week 3 Assignment 2
## Caching the inverse of a matrix
## The two functions below are used to create a special object that stores a matrix
## and caches its inverse

## The function below creates a special "matrix" object that can cache its inverse
## set & get the value of the matrix
## set & get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function()inver
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}

## The function below computes the inverse of the special "matrix" created above
## But it will first check to see if it's inverse has already been calculated
## If so, it will get its inverse from the cache
## Else, it will compute its inverse and set its inverse in the cache

cacheSolve <- function(x, ...) {
        inver <- x$getInverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setInverse(inver)
        inver
}

## The End