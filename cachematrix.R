##################################################################################################
## R Programming - Week 3 Assignment - Miruna Serman
##################################################################################################
## Creates an R object that stores a Matrix and its inverse. It also builds a set of functions
## and returns the functions within a list to the parent environment.

## Variables and paramaters
# inv - variable that holds the inverse
# x - matrix
# inverse - parameter that is passed in
# list - creates a list with the setter and getter functions.

##Note: <<- operator assigns the value on the right side of the operator to an object
##in the parent environment named by the object on the left side of the operator.

##################################################################################################
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        
    }
    get <- function()
        x
    setInverse <-
        function(inverse)
            inv <<-
        inverse #in_inv is a parameter (inversed matrix) being passed in
    getInverse <- function()
        inv
    list(
        set = set,
        # gives the name 'set' to the set() function defined above
        get = get,
        # gives the name 'get to the get() function defined above
        setInverse = setInverse,
        # gives the name 'setInverse' to the setInverse() function defined above
        getInverse = getInverse # gives the name 'getInverse' to the getInverse() function defined above
    )
}

##################################################################################################
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cach

## Variables, paramaters and functions
## x  - output of makeCacheMatrix()
## matrix.data - variable that holds the matrix data
## inv - variable that holds the inverse.
## Solve - function calculates the inverse of a square matrix
##################################################################################################

cacheSolve <- function(x, ...) {
    #Returns a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    #if the inverse has been calculated, gets if from cached data
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
        
    }
    
    # otherwise, calculates the inverse
    matrix.data <- x$get()
    inv <-
        solve(matrix.data , ...)  #Solve function calculates the inverse of a square matrix
    
    # sets the value of the inverse in the cache via the setInverse function.
    x$setInverse(inv)
    return(inv)
    
    
}

##################################################################################################
##Testing the functions
##################################################################################################

##Test
a  <- matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2)
b <- makeCacheMatrix(a)
cacheSolve(b)
cacheSolve(b) # Second function call will cache data

##Test
set.seed(9)
r = rnorm(9)
mat = matrix(r, nrow = 3, ncol = 3)
b <- makeCacheMatrix(mat)
cacheSolve(b)
cacheSolve(b) # Second function call will cache data

##Test
a  <- matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2)
cacheSolve(makeCacheMatrix(a))
cacheSolve(makeCacheMatrix(a)) # This will not cache because it's a separate call

##Test
cacheSolve(makeCacheMatrix(matrix(
    c(2, 4, 1, 3), nrow = 2, ncol = 2
)))
cacheSolve(makeCacheMatrix(matrix(
    c(2, 4, 1, 3), nrow = 2, ncol = 2
))) # This will not cache becaue it's a separate call


##Test
# set.seed(1110201)
# r = rnorm(1000000)
# a = matrix(r, nrow = 100, ncol = 100)
# b <- makeCacheMatrix(a)
# cacheSolve(b)
# cacheSolve(b)# Second function call will cache data

##Test
a <- diag(5, 3) #Creates a diagonal matrix
CachedMarix <- makeCacheMatrix(a)
cacheSolve(CachedMarix)
cacheSolve(CachedMarix)# Second function call will cache data
