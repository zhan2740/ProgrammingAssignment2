##############################################################################
####FIRST, I DO NOT THINK THIS IS A GOOD ASSIGNMENT FOR A NEW R LEARNER!!!####
####SECOND, I DO NOT THINK THIS COURSE WERE WELL ORGANIZED.               ####
####THIRD, I ALMOST GOT NO TRAINING DURING THE COURSE. THE EXAMPLES ARE   ####
####BAD. THEY ARE NOT RELATED TO PRATICAL QUESTIONS OR APPLICATION.       ####
####THUS, I STRONGLY RECOMMEND NOT TO TAKE THIS COURSE. BAD EXPERIENCE!!! ####
##############################################################################


## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that cache the inverse of a matrix.

## 1. makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## Actually creat a list containing four functions

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL              # create local maxtrix "invM" (inverse Matrix) and set it to NULL 
        set <- function(y) {      # "set" function that will store the values of x and "invM"
                x <<- y           # setting "y" as the x value
                invM <<- NULL     # initializing "invM" to NULL 
        }
        get <- function() x       # function "get" that will return the value of x when called 
                                  # function "setInverse"that will set the value of "invM" to the 'inverse' argument passed to it 
        setInverse <- function(inverse) invM <<- inverse
                                  # fucntion "getInverse" that will return the value of "invM" 
        getInverse <- function() invM
                                  # return a list with each of the new functions saved to its own name
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2. cacheSolve: This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                                  # return a matrix that is the inverse of 'x'
        invM <- x$getInverse()    # first, save the $getinverse value to "invM" so we can check it.
                                  # if "invM" is not null, we have already calculated the inverse of this matrix 
        if (!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
                                  # if "invM" is null, we will need to calculate the inverse 
        mat <- x$get()            # get the matrix from the list using $get and store it as "mat"  
        invM <- solve(mat, ...)   # use the "solve" function to calculate the inverse of "mat"
        x$setInverse(invM)        # store the newly calculated inverse to '"nvM" using the "setinverse" function 
        invM                      # return the value of "invM" to the user
}

