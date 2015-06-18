## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeCacheMatrix creates a special "matrix" object that 
## can cache its invese.  
## 

makeCacheMatrix <- function(x = matrix()) { # the function takes a matrix as input
  inv <- NULL
  set <- function(y) { # set(y) will pass y value to x, and saved in a special environment 
    x <<- y
    inv <<- NULL #inside the set function, inv is set to be NULL
  }
  get <- function() x # this will get the x value
  setinverse <- function(inverse) inv <<- inverse # this assign the inverse value to inv and saved in a special environment
  getinverse <- function() inv # this retrieves the inverse value, which is inv.
  list(set = set, get = get,
       setinvere = setinverse, getinverse = getinverse) #the function returns a list of varibles
}


## Write a short comment describing this function
## Function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## in inverse from the cache.

cacheSolve <- function(x, ...) { #this function takes a list generated above as input
  inv <- x$getinverse() # check to see whether there is a cached inv saved in the special environment
  if(!is.null(inv)) { #if yes, then return the saved inverse value, and throw a message.
    message("getting cache data")
    return(inv)
  }
  data <- x$get() #if not, then get the actual matrix
  inv <- solve(data,...) # calculate the inverse of the matrix
  x$setinv(inv) #save the inverse value to the cache
  inv #return the inverse value
}
