## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix will create a list composed of two elements:
## the original n x n matrix, and its inverse
## List will be saved as a "global object" so it will be available in the 
## in the environment as a cache
makeCacheMatrix <- function(x = matrix()) {
    cache<<-list(x, NA)
}


## Function cashSolve will get a n x n matrix and it will return its inverse
## Verify if the matrix in the cache is still the same
## It will verify if its inverse is already cached in the global list
## It will calculate the inverse and save it to the cache list
## It will return the inverse from the cache list

cacheSolve <- function(x, ...) {
    ## Define function to update cache
    updateCache <- function(x) {
        ## Move the new matrix to the cache, 
        cache[[1]]<<-x
        ## Calculate inverse and save into cache
        cache[[2]]<<-solve(x)
    }
    ## if matrix in the cache has different dimensions from x, update cache
    if (!all(dim(x) == dim(cache[[1]]))) {
        updateCache(x)
    } else {
        ## If x is different from cache or if inverse is not in cache, update
        ## cache
        if (any(x != cache[[1]], is.na(cache[[2]]))) {
            updateCache(x)
        }
    }
    ## Return the inverse in cache
    cache[[2]]
}