## Put comments here that give an overall description of what your
## functions does

## makeCacheMatrix is able to have an matrix input by variable x
## Sets the "inv" variable to NULL at start
## and defines functions to set x, get x, set inv and get inv
## cacheSolve is able to have a varialble input in which the makeCacheMatrix function is defined with a certain matrix variable x
## It either calculates the inversion matrix or takes it from the cache when the inversion is already cached for the same input

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   #creates function with matrix as input
        inv <- NULL                           #sets inv variable to NULL
        set <- function(y) {                  #creates the set function
                x <<- y                       #puts y into x thus setting x to NULL again
                inv <<- NULL
        }
        get <- function() x                   #creates the get variable with function definition to get x
        setinv <- function(inversion) inv <<- inversion  #creates the setinv function to put the calculated inv into the makeCacheMatrix cache
        getinv <- function() inv              #creates the getinv variable with function definition =inv
        list(set = set, get = get,            #creates the list names coupled to the variable with the same names
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                    #creates the parent function env which expects a variable in which the makeCacheMatrix function are defined for a certain matrix input
        inv <- x$getinv()                           #gets the matrix from makeCacheMAtrix
        if(!is.null(inv)) {                         #checks if the matrix is not NULL, if so then the cache is used and retruns the cached result
                message("getting cached inversion")
                return(inv)
        }
        data <- x$get()                             #else get the matrix from the makeCacheMAtrix function input and store it in the variable data   
        inv <- solve(data, ...)                     #solve(inverse) the matrix in the variable data to get the inverse
        x$setinv(inv)                               #set the inversed matrix in inv variable into the cache of the cached makeCacheMatrix function
        inv                                         #print the inversed matrix
}
        ## Return a matrix that is the inverse of 'x'
