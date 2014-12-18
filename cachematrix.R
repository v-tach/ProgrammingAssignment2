## Put comments here that give an overall description of what your
## functions do

## This function will create a list functions
## When the function is applied to a matrix
## it will save the matrix

makeCacheMatrix <- function(x = matrix()) {
		# set the cache to NULL initially
		inverse.cache <- NULL
		# create a function to set the value of the matrix
		set <- function(y) {
			x <<- y
			inverse.cache <<- NULL
		}
		# create a function to get the matrix
                get <- function() x
        # create a function to solve  the inverse
		set.cache <- function(solve) inverse.cache <<- solve
         # create a function to cache the inverse
			get.cache <- function() inverse.cache
		# make a list that holds the helper functions,
        # and which in turn hold the cache value 
            list(set = set, 
				get = get,
				set.cache = set.cache,
				get.cache = get.cache)
		}


## This function takes the list above and pulls out
## the cache  of the inverse of the matrix (if it exists),
## or uses the functions in the list to calculate the inverse
## and cache the results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	# pull the inverse from the list
        inverse.cache <- x$get.cache()
                # test to see if the cache has been calculated
        	if(!is.null(inverse.cache)) {
                        # if it has been calculated, use it
        		message("getting cached data")
        		return(inverse.cache)
        	}
                # if the cache is still null, calculate the inverse
                #get the data
        	data <- x$get()
                # solve for the inverse
        	inverse.cache <- solve(data, ...)
                # put the solution in the cache
        	x$set.cache(inverse.cache)
        	return(inverse.cache)
}
