## The function makeCacheMatrix create a matrix that can be stored in cache.
## Function cacheSolve computes the inverse of the matrix returned by 
## makeCacheMatrix - if the inverse has already been calculated, then the 
## cacheSolve returns the inverse from the cache. 


## The following function creates an empty matrix with four functions: set,
## get, setinverse and getinverse. In general terms it sets up a base for the
## computation of the inverse matrix in the cacheSolve function. 

makeCacheMatrix <- function(x = as.matrix()) {
		m <- NULL
        set <- function(y = as.matrix()) {     ## Setting matrix x
                x <<- y
                message("setting cached inverse")
                m <<- solve(y) 
        }
        get <- function() x    ## Getting matrix x
        setinverse <- function(inverse) m <<- inverse ## Setting inverse of matrix x
        getinverse <- function() m   ## Getting inverse of matrix x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) ## Returning list with four arguments which I defined above
           
}


## cacheSolve function creates an inverse of matrix x. If the inverse has been already computed in the makeCacheMatrix i.e. "m" returns a number,
## then the function returns the inversed matrix from cache, otherwise it computes the inverse of matrix x.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  ## Returning inverse of matrix x (if it has already been computed) 
        }
        data <- x$get()
        m <- solve(data)
        message("setting inverse") 
        x$setinverse(m) ## If the inverse has not been computed, then this line computes the inverse of matrix x
        m
}
