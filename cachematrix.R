## These functions will allow  the inverse of a matrix to be 
## calculated, stored, and retieved from cache. The first functionm makeCacheMatrix
## creates the input for cacheSolve.

## The first function creates a matrix "object" that is a list of 4 functions
## and 2 data objects that are returned to the parent environment.The intput is a
## numeric matrix a

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL #The inverse is initialized as null
#the first function assigns the input argument to the a object in the parent
# environment and sets inv to NULL, wiping out whatever was there by previous
# calls to the subsequent function cacheSolve.
  set <- function(y) {
    a <<- y
    inv <<- NULL
  }
# The next function in the returned list gets the matrix
  get <- function() a
# The next function sets the inverse object, inv, and uses
# <<- as the assignment operator so that the value if the parent
# environemnt is used
  setinv <- function(solve) inv <<- solve
# Retrieve the appropriate value of the inverse
  getinv <- function() inv
# And finally, set the output list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function takes in the object (list) from the above
## subroutine, checks to see if its inverse is in cache and if not, calculates and 
## returns the inverse. If it the inverse IS in cache (i.e. not NULL) then it retrives
## and returns that value


cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a' 
  inv <- a$getinv() #gets the matrix
#  print(inv) # This was me checking if it was NULL!

# If the inv is NOT NULL, that means you already computed the inverse and
# it is stored! yay!
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
# Otherwise, you need to find the inverse. This is done by
# retrieving the matrix and using the solve function
# to get compute it
# Then, set the value of inv so the next time, it is stored.
  x <- a$get()
  inv <- solve(x, ...)
  a$setinv(inv)
  inv
# I test this code by running it twice in a row--the first time, the 
# "getting cahced data" message was not shown since the inverse was not
# computed and the second time it was called, it successfully found the
# inverse in cache
}
