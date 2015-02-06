## Program Assigment 2: Caching the Inverse of a Matrix
## A pair of functions that cache the inverse of a matrix:
##  1.  `makeCacheMatrix`: This function creates a special "matrix" object 
##      which is really a list.
##  2. `cacheSolve`: This function computes the inverse of the special
##      "matrix" returned by `makeCacheMatrix` above.
## Note: assume that the matrix supplied is always invertible.

## `makeCacheMatrix`: This function contains four functions:
##      (1) `set`: set values of the matrix
##      (2) `get`: get values of the matrix
##      (3) `setinverse`: set the inverse value of the matrix
##      (4) `getinverse`: get the inverse value of the matrix

## Test:
##  x <- makeCacheMatrix(diag(1:3,3,3))
##  cacheSolve(x)
##      #      [,1] [,2]      [,3]
##      # [1,]    1  0.0 0.0000000
##      # [2,]    0  0.5 0.0000000
##      # [3,]    0  0.0 0.3333333
##  cacheSolve(x)
##      # getting cached data
##      #       [,1] [,2]      [,3]
##      # [1,]    1  0.0 0.0000000
##      # [2,]    0  0.5 0.0000000
##      # [3,]    0  0.0 0.3333333

makeCacheMatrix <- function(x = matrix()) {
    # initial the inverse value
    inverse <- NULL
    # `set` function
    set <- function(y){
        # clear inverse value
        inverse <<- NULL
        # set matrix value
        x <<- y
    }
    # `get` function
    get <- function() x
    # `setinverse` function
    setinverse <- function(inversevalue) inverse <<- inversevalue
    # `getinverse` function
    getinverse <- function() inverse
    # Output
    return(list(set=set,get=get,setinverse=setinverse,getinverse=getinverse))
}


## `cacheSolve`: This function is to obtain the inverse of a "matrix" created by `makeCacheMatrix`
##  if the inverse has been calculated, the inverse will be obtained from the cache
##  else it will calculate the inverse

cacheSolve <- function(x, ...) {
        inversevalue <- x$getinverse()
        if(!is.null(inversevalue)){
            # directly get the inverse
            message("getting cached data")
            return(inversevalue)
        }else{
            # Calculate the inverse
            data <- x$get() # Get the matrix
            inversevalue <- solve(data, ...) # Solve the inverse
            x$setinverse(inversevalue) # Store the inverse
            return(inversevalue) # Output the inverse
        }
}
