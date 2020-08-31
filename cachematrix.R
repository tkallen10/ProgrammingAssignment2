## First one takes invertable square matrix, adds functions, and is used to assign it a name
## Second one takes named matrix and uses functions to check for an inverse already in 
##    the parent environment, and returns it if it can find it.  Otherwise it calculates
##    the inverse and moves data and inverse to parent environment.

#  Accepts a matrix, creates functions, and names them in a list
#  When then object is stored and used the functions are available to the function 
#     calling the object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                  #initializes inv in parent environment to null as object is created
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(whatever) inv <<- whatever
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
#  This is the function that is used to call the objects created by the function above
#  It calls functions stored in the list of the object that is passed to it

  cacheSolve <- function(x, ...) {
    inv <- x$getInverse()        # Checks for value for inv in the parent environment
                                 # and stores it if it finds
    if(!is.null(inv)){           # If if finds it, sends message and returns it
      message("getting cached data")
      return(inv)
    }
    data <- x$get()              # If it couldnt find it, moves original matrix to parent
    inv <- solve(data)           #   and calculates invervse
    x$setInverse(inv)            #   and moves inverse to parent environment
    inv      
  }
