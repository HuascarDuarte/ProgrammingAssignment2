## Function makeCacheMatrix will receive as argument a matrix x and than it will
## return a list that will contain the matrix and its inverse

## The list will be composed of 4 elements:
## $get: Element $get contains a function that returns the contents of the 
##       variable x which exists inside the list
## $set: Element $set contains a function that will initilize the variables x
##       (should) contain the original matrix) and xinv (should contain the 
##       inverse of x). Both variables will exist only inside the instance of the list created.
## $setinv: Element $setinv is a function that will populate xinv with its 
##          argument which is expected to be x's inverse
## $getinv: Element $getinv will return the contents of xinv stored in the list
##
makeCacheMatrix <- function(x = matrix()) {
    ## initialize variable xinv
    xinv <- NULL
    ## $set: variable with function to populate x (original matrix)
    ## and xinv (inverse of the matrix) as variables that will exist only inside
    ## the function and inside the list. The values of x and xinv will persist
    ## inside the list
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    
    ## $get: variable with function to return the value of x which is the 
    ## original matrix
    get <- function() x
    
    ## $setinv: variable with function the will save its argument in the list
    ## variable xinv. This argument should be the inverse of x.
    setinv <- function(inv) xinv <<- inv
    
    ## $getinv: variable with function to return the value of xinv which is the
    ## inverse of x
    getinv <- function() xinv
    
    ## create and return the list. 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Function cacheSolve receive as argument the list created by function 
## makeCacheMatrix and will compute and return its inverse from the cache if it
## was already stored in the list.
##
## It will verify if the inverse of the original matrix x is
## already stored inside the list (calling a function defined in the element
## $getinv).
## If it is already there, 
##     it will return the inverse which is already in the list. 
## Otherwise, 
##     it will get the original matrix from the list, calling a function
##     defined in element $get of the list.
##     it will calculate the inverse of the original matrix
##     it will save the inverse of the matrix by calling a function defined
##     in element $setinv of the list
##     it will return the inverse matrix calculated
cacheSolve <- function(x, ...) {
    ## Get the inverse original matrix from the list
    m <- x$getinv()
    ## If the inverse is already computed (not null)
    if(!is.null(m)) {
        ## Message saying the inveser comes from the cache
        message("getting cached data")
        ## Returns the inverse and exits
        return(m)
    }
    ## If function hasn't exited, inverse hasn't been computed
    ## Get the original matrix from list
    data <- x$get()
    ## Compute inverse
    m <- solve(data)
    ## Store inverse in the list
    x$setinv(m)
    ## Returns inveser
    m
}