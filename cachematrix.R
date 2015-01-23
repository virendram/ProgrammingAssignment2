## Assignment 2: R_Programming on Coursera
## Author:Virendra R Mishra

## This function uses <<- operator to create cache so that the objects are not exposed
## It uses 4 internal functions: set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # use this to set a matrix to object created by makeCacheMatrix function
        # e.g makeCacheMatrix(testmatrix) # here we work on testmatrix
        # makeCacheMatrix$set(testmatrix1) # here we work on testmatrix1
        
        set <- function(y) {
                x <<- y
                m <<- NULL # it also initialises m to null
        }
        
        get <- function() x # return the input matrix
        setinv <- function(inv) m <<- inv # set the inverse matrix
        getinv <- function() m # return the inverse matrix
       
        
        # return a list that contains these functions, so that we can use
        # makeCacheMatrix object like these
        # x <- makeCacheMatrix(testmatrix)
        # x$set(newmatrix) # to change matrix
        # x$get # to get the setted matrix
        # x$setInv # to set the inversed matrix
        # x$getInv # to get the inversed matrix
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) 

}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()  # get the inverse matrix from object x
        # it will be null if uncalculated, remember the first line "m<- NULL" in the previous function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  # return the calculated inversion
        }
        data <- x$get() # if not, we do x$get to get the matrix object
        m <- solve(data, ...) # we solve it
        x$setinv(m) # we then set it to the object
        m # return the solved result
}
