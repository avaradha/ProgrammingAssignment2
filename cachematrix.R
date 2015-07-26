# The two functions shown below creates an object to store the matrix and cache the
# inverse of the same


# Function makeCacheMatrix does the following:
# 1. Sets the object to the new matrix 
# 2. Gets the set matrix object
# 3. Sets the inverse of the matrix object
# 4. Gets the object that contains the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    assign(x, y, inherits = TRUE)
    assign(inv, NULL, inherits = TRUE)
  }
  get <- function() x
  setInverse <- function(inv_mat){
    inv <<- inv_mat
  }  
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function cacheSolve does the following:
# 1. Checks if the inverse of the matrix object is cached
# 2. If yes, then it returns the inverted matrix
# 3. If not, it performs the inverse and then caches the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Cached Inverse Found...Getting Cached Inverse...")
    print(inv)
    return(inv)
  }
  message("No Cached Inverse Found...Performing the Inverse...")
  mat_data <- x$get()
  inv <- solve(mat_data)
  x$setInverse(inv)
  message("Inverted Matrix is:")
  print(x$getInverse())
}
