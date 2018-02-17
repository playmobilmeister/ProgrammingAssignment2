## two functions that are used to create a special object that stores a
##numeric matrix and caches its inverse.


## stores a numeric matrix

makeCacheMatrix <- function(x = matrix()) {
       
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) m <<- inverse
                getinverse <- function() m
                list(set = set, get = get, setinverse = setinverse,
                     getinverse = getinverse)
        
}
## return inverse of a matrix
cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set(m)
        m
}
       



#The operators <<- and ->> are normally only used in functions, 
#and cause a search to be made through parent environments for 
# an existing definition of the variable being assigned. 
# If such a variable is found (and its binding is not locked) 
# then its value is redefined, otherwise assignment takes place 
#in the global environment. Note that their semantics differ from 
#that in the S language, but are useful in conjunction with the scoping
# rules of R. See 'The R Language Definition' manual for further details
#and examples.