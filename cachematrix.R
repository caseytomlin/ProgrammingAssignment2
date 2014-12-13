## makeCacheMatrix() takes an optional matrix argument
## and returns a list of 4 functions, called set(),
## get(), setinv(), and getinv(), which together constitute
## a sort of fancy matrix object capable of representing both
## a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL   # Initialize the inverse of x to be NULL
        set <- function(y) {      # set() allows one to (re)specify 
                x <<- y           # a matrix for an existing
                xinv <<- NULL     # makeCacheMatrix() object, and
        }                         # initializes its inverse to NULL
        # the use of <<- in set() allows assignment outside the body
        # of set(), i.e. to the xinv on line 15 and the x returned by
        # get() below.
        
        get <- function() x       # returns the matrix x
        
        # setinv() allows the manual specification of the inverse,
        # either by hand or using the cacheSolve() function below.
        setinv <- function(inv) xinv <<- inv  
        
        getinv <- function() xinv # return the inverse
        
        # return a list of these functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve() takes a makeCacheMatrix() argument,
## computes the inverse of the corresponding matrix if it
## has not yet been computed, and sets it as the inverse
## in the makeCacheMatrix() object. If the inverse of the
## makeCacheMatrix() matrix has already been set, then
## cacheSolve() simply returns this value.

cacheSolve <- function(x, ...) {
        xinv <- x$getinv()   # Extract the inverse from a 
                             # makeCacheMatrix() object x.
        
        if(!is.null(xinv)) {  # If xinv isn't NULL, return its value.
                message("getting cached data")
                return(xinv)
        }
        
        # Otherwise, compute the inverse using solve()...
        data <- x$get()
        xinv <- solve(data, ...)
        
        # and set the value of the inverse manually, and return it
        x$setinv(xinv)
        xinv
}
