## The following functions store an inverse of a squared matrix in the chache memory. 
## If the inverse of the matrix already exists the CacheSolve Function get this inverse from the past function 
## functions do

## This function gets ans stores the inverse of the squared matrix

makeCacheMatrix <- function(x = matrix()) {
  
              m <- NULL
              set<- function (y){
                x<<-y
                m<<-NULL
              }
              get <- function() x
              setinverse <- function (solve) m <<- solve
              getinverse <- function() m 
              list (set=set, get=get, setinverse=setinverse,
                    getinverse=getinverse)
}
y=matrix(c(2,4,2,3),2,2,byrow = T)
x=makeCacheMatrix(y)
class(x)



## This functions reviews if ans inverse already exist in the cache memory, if so it
## Returns a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
            m <- x$getinverse ()
            if (!is.null(m)) {
              message("getting cached data")
              return (m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse (m)
            m
}
cacheSolve(x)
solve(y)
