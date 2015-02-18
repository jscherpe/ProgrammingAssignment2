## Below are two functions that are used to create an object that stores a matrix and cache's its inverse.
## As such, the inverse can be looked up when used repeatedly and does not have to be computed every time, thereby saving computational time. 

## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse. This "matrix" object is a list containing a function to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse (setinverse)
## 4. get the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## the set function        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        ## the get function
        get <- function() x
        ## the setinverse function
        setinverse <- function(inverse) i <<- inverse
        ## the getinverse function
        getinverse <- function() i
        ## the list returned
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The "cacheSolve" function calculates the inverse of the special "matrix" created with the first function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}