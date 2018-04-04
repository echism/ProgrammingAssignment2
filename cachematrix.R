## This is a constructor that makes an object from a matrix that caches its inverse
## Running tips:
## The makeCacheMatrix function takes a matrix as a parameter but not all matrices are suitable for inversion
## The matrix must be square (i.e. have an identical number of rows and columns)
## if it is not square, cacheSolve will throw an error listing your matrix dimensions and saying "must be square"
## The matrix must not be a singular matrix, which is not invertible. 
## It's up to you whether you want to brush up on what a singular matrix is or to do calculations to prevent a singular matrix
## but in practical terms, if you happen to make one and run it through makeCacheMatrix, running cacheSolve will return an error
## the error will mention "system is exactly singular" 
## an easy way to make a proper matrix is to make a 2x2 matrix where the product of the diagonal values is not equivalent
## demonstrated:
## singular matrix: matrix(c(3,9,2,6),2,2)                    
##    | 3 2 | 3*6 = 9*2 
##    | 9 6 | cannot be inverted
##  invertible matrix:  matrix(c(3,9,5,6),2,2)                    
##    | 3 5 | 3*6 != 9*5 
##    | 9 6 | can be inverted
## bigger matrices have more complex formulas, ad-bc does not work for 3x3 or bigger matrices

## running properly, this returns a list of functions 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## run properly, this returns a matrix (an inverted one)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

## example run with output:
## b<-matrix(c(1,6,4,99,29,30,6,800,1),3,3)
## c<-makeCacheMatrix(b)
## cacheSolve(c)
##              [,1]          [,2]         [,3]
##[1,] -0.0819188091  0.0002768105  0.270064487
##[2,]  0.0109152174 -0.0000786005 -0.002610904
##[3,]  0.0002187144  0.0012507732 -0.001930838

