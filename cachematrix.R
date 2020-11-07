## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment
##
## Sample run-time :
## source('~/Learning_R/cacheMatrix.R', encoding = 'UTF-8')
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$get()
## my_matrix$setInverse()
## cacheSolve(my_matrix)
##
## Sample run-time results :
## > source('~/Learning_R/cacheMatrix.R', encoding = 'UTF-8')
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## > my_matrix$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > my_matrix$setInverse()
## > cacheSolve(my_matrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
#
#
makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        # create the matrix 
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }

        # get the value of the matrix
        get <- function() x
        # invert the matrix with solve and store in cache  
        setMatInverse <- function() cache <<- solve(x)
        # get the inverted matrix from cache
        getMatInverse <- function() cache
        # return the created functions
        list(set = set, get = get, setInverse = setMatInverse, getInverse = getMatInverse)      

}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## Creates in the working environment and stores it's inverted value in cache
#

cacheSolve <- function(x, ...) {
      cacheSolve <- function(x, ...) {
## gets the inverse of the matrix stored in cache
   cache <- x$getInverse()
## checks if it is not null returns inverted matrix from cache
   if(is.null(cache)) {
      message("notgettingcacheddata")
      return(cache)
   }
## gets matrix
   mat<−x$get()
## sets inverted matrix in cache
   inv<- x$setInverse()
## gets the inverted matrix in cahce   
   cach<- x$getInverse()
## function returns the inverted cache matrix
   return(cach)
}
