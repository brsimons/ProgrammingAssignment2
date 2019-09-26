## This function creates a matrix with the object m. Using solve inverts this matrix. The actual output of this function is a list. 
## In the function set() the object the value NULL is assigned to m in the parent environment.Afterwards result of the solve function 
## is assigned to m.
 
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL()
        }
        get <- function() x
        setinversematrix <- function(solve) m <<- solve
        getsolve <- function() m
        list(set=set, get=get,
             setinversematrix=setinversematrix,
             getsolve=getsolve)
}        
        

## The function cacheSolve first call the x$getsolve function from the makeCacheMatrix. It checks if the m is NULL, if this is not the case
## the inverse matrix is solid and given back to the parent environment. 
## Output of this function is the inverse matrix of x.         
        
cacheSolve <- function(x, ...) {
        m <-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}