## Put comments here that give an overall description of what your
## functions do

## Hey guys! So for this assignment I basically used the backbone of the provided code and changed the function to inverse the matrix,
## because it already looked like the easiest way to do it and the purpose of the task was to understand how to make closures/ program
## Object oriented (Easiest way would have been to write this in python, no?)

## This function works as a class (or wrapper) it includes all functions you can want to use in the environment you create
## (coming from python I would say that it creates the instance of a class)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){ # This function is kind of a resetter, the matrix is assigned to x in the parent environment
                x<<-y 
                m<<-NULL
        }
        get <- function() x # returns matrix
        setsolve <- function(solve) m <<- solve # calculates inverse of matrix and assigns it to m in parent environment
        getsolve <- function() m # returns inverse of matrix
        list(set=set, get=get, # This list includes all the functions you need for cacheSolve and makes them available by env$function
                setsolve=setsolve,
                getsolve=getsolve)
}


## cacheSolve first checks if you already have executed cacheSolve one, or if you already calculated the inverse of you matrix or not
## if not, the original matrix is retrieved and stored in data (by executing what we prepared in makeCacheMatrix)
## then call the inverse matrix (solve) function on the matrix and assigning it's value to m. This is transferred to the parent env by setsolve
## Afterwards m is returned, you got your inversed matrix!
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get() 
        m<- solve(data, ...) # Solve function is applied on matrix
        x$setsolve(m) # The inverted matrix is assigned to the environment x
        m # The inverted matrix is returned
}

