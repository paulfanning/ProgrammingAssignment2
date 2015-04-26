# Paul Fanning
#
##
# The two functions create and/or use a structured list to cache the inverse of a matrix
# Use of lexical scoping allows values to be stored in the environment of the list

##  makeCacheMatrix
#Creates a structured list of the data matrix, the inverse (initially NULL) and functions to
# set and get both matrices
#
# To allow the solver to detect a change in the input matrix the function also creates a value newmat (initially TRUE)
# The solve function then re-sets this to FALSE when it solves for and caches an inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv<- NULL
      newmat<-TRUE
      set<- function(y) {
            x<<-y
            inv<<-NULL
            newmat<<-TRUE
      }
      get<- function() x
      setinv<- function(tinv) {
            inv<<-tinv
            newmat<<-FALSE
      }
      getinv<- function() inv
      check<- function() newmat
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv, check = check)
}


## cacheSolve
# If the newmat variable in the input list FALSE
# the function returns the cached inverse value
# otherwise it solves for the inverse, caches it and
# sets the newmat variable to FALSE

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
      if(!x$check()) { # if this is this not a new matrix
            message("inverse cached")
            return(x$getinv())
      }
      mat<-x$get()
      inv<- solve(mat, ...)
      x$setinv(inv) # includes setting newmat to FALSE
      inv
}

### Test lines below here

tdat<-matrix(c(8,4,6,3,7,2,3,5,9),c(3,3))

n=5
otherdat<-matrix(rnorm(1:(n*n)), nrow=n, ncol=n)

mvec<-makeCacheMatrix(tdat)
mvec<-makeCacheMatrix(otherdat)

cacheSolve(mvec)

