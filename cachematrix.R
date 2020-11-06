## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {                 #function to cache inverse matrix
  inversematrix <- NULL                                      
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get               <- function()     x
  set_inversematrix <- function(inversem) inversematrix <<- inversem
  get_inversematrix <- function()     inversematrix
  list(set = set, 
       get = get,
       set_inversematrix = set_inversematrix,
       get_inversematrix = get_inversematrix
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                         #conditional function
  inversematrix <- x$get_inversematrix()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$set_inversematrix(inversematrix)
  inversematrix
}

#EXAMPLE
a <- c(4,5,4)
b <- c(3,4,4)
c <- c(8,7,7)


B <- rbind(a,b,c)

cacheSolve(makeCacheMatrix(B))
