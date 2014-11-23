#This function creates a special "vector" object of functions that
#store the inverse to cache(set)
#get the data from a matrix(get)
#compute the inverse and stores it to cache (setinverse)
#retreive the inverse from cache (getinverse)
#compute the inverse, and 
#makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix) m <<- solve
        getinverse <- function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)      
}


#This function uses the special vector returned by 
#makeCacheMatrix to determine if the inverse has already been calculated (and the matrix
#has not changed). If so, the function willThen cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<- x$getinverse()
	if (!is.null(m) && m == x ) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}