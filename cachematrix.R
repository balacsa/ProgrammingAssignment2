## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix makes a matrix that can cache its inverse
## acheSolve function gives the cached inverse matrix if is available, if not it is calculate it and cache it via makeCacheMatrix function

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL                          ## initialize the inverseMatrix
        set <- function(y) {						   ## define set function to assing value of matrix from the parent environment and set the inverseMatrix to NULL
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x                            ## define get function to return vaule of matrix
        setInverseMatrix <- function(solve) inverseMatrix <<- solve  ##assigns value of inverseMatrix in parent environment
        getInverseMatrix <- function() inverseMatrix   ## gets vaule of inverseMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverseMatrix()          ##  gets the inversematrix from cachematrix
        if(!is.null(inverseMatrix)) {                  ##  if cachematrix is available gives a message and gives back the cached inversematrix
                message("getting cached matrix")         
                return(inverseMatrix)                   
        }
        originalMatrix <- x$get()                      ## get the original matrix data
        inverseMatrix <- solve(originalMatrix, ...)    ## calculates the inverse matrix
        x$setInverseMatrix(inverseMatrix)              ## puts the calculated inverse matrix into the cache matrix
        inverseMatrix                                  ## gives back the calculated inverse matrix
}