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
## It will verify if the matrix in the cache list is the same
## It will verify if its inverse is already cached in the global list
## It will calculate the inverse and save it to the cache list
## It will return the inverse from the cache list

cacheSolve <- function(x, ...) {
    ## Verify if the matrix in the cache has changed or inverse is not in cache
    if (any(x!= cache[[1]], is.na(cache[[2]]))) {
        ## Calculate inverse and save into cache
        cache[[2]]<<-solve(x)
    }
    ## Return a matrix that is the inverse of 'x'
    cache[[2]]
}
