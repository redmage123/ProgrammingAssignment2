## Put comments here that give an overall description of what your
## functions do



# makeCacheMatrix()
# This function takes a matrix as an argument (specifies an empty
# matrix if none is passed).
#
# It creates four inner functions.
# 1.  The set() function sets the current matrix to be solved. 
#     as well as initializing the inverse matrix variable
# 2.  the get() function retrieves the current matrix.
# 3.  the setMatrix() function solves for the inverse 
# 4.  the getMatrix() function returns the inverse. 
# 
# Finally, the function returns a list of the functions so that they 
# can be called from an external calling entity. 

makeCacheMatrix <- function (x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setMatrix <- function(solve) {
        m <<- solve
    }
    getMatrix <- function() {
        m
    }
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


# cacheSolve()
# This function tries to solve for the inverted matrix
# However, since the solve() function is computationally
# expensive we first see if the solution has already been
# computed and cached.  If so, let's use that one rather than
# re-invent the wheel.  Otherwise, we'll solve the matrix
# and cache it using the setMatrix() function. 

cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if (! is.null(m)) {
        message ("Getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix,...)
    x$setMatrix(m)
    m
}

#m <-matrix(c(-1,-2,1,1),2,2)
#a <- makeCacheMatrix(m)
#a$get()
#inv <- cacheSolve(a)
#inv
#inv <- cacheSolve(a)
#inv
