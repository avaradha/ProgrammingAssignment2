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
    message("\nCached Inverse Found...Getting Cached Inverse...\n")
    print(inv)
    return(inv)
  }
  message("\nNo Cached Inverse Found...Performing the Inverse...")
  mat_data <- x$get()
  inv <- solve(mat_data)
  x$setInverse(inv)
  message("\nInverted Matrix is:\n")
  print(x$getInverse())
}

####################
#     SAMPLE RUN    
####################

# First Run for a 2D Matrix 
x = rbind(c(1, 1/2), c(1/4, 1))
m = makeCacheMatrix(x)
cat("\nInput Matrix is:\n")
print(m$get())
start <- proc.time()
cacheSolve(m)
cat("\nTime Taken:\n")
print(proc.time() - start)

# Second Run for the 2D Matrix
start <- proc.time()
cacheSolve(m)
cat("\nTime Taken:\n")
print(proc.time() - start)

# First Run for a 3D Matrix 
r1 <- c(1, 1/2, 1/3)
r2 <- c(1/2, 1, 1/3)
r3 <- c(1/3, 1/2, 1)
x <- rbind(r1, r2, r3)
m = makeCacheMatrix(x)
print("Input Matrix is:")
print(m$get())
start <- proc.time()
cacheSolve(m)
cat("\nTime Taken:\n")
print(proc.time() - start)

# Second Run for the 2D Matrix
start <- proc.time()
cacheSolve(m)
cat("\nTime Taken:\n")
print(proc.time() - start)

############################
#   OUTPUT OF SAMPLE RUN    
############################

# > source('~/github/ProgrammingAssignment2/cachematrix.R')
# 
# Input Matrix is:
#   [,1] [,2]
# [1,] 1.00  0.5
# [2,] 0.25  1.0
# 
# No Cached Inverse Found...Performing the Inverse...
# 
# Inverted Matrix is:
#   
#   [,1]       [,2]
# [1,]  1.1428571 -0.5714286
# [2,] -0.2857143  1.1428571
# 
# Time Taken:
#   user  system elapsed 
# 0.004   0.003   0.125 
# 
# Cached Inverse Found...Getting Cached Inverse...
# 
# [,1]       [,2]
# [1,]  1.1428571 -0.5714286
# [2,] -0.2857143  1.1428571
# 
# Time Taken:
#   user  system elapsed 
# 0.001   0.000   0.001 
# [1] "Input Matrix is:"
# [,1] [,2]      [,3]
# r1 1.0000000  0.5 0.3333333
# r2 0.5000000  1.0 0.3333333
# r3 0.3333333  0.5 1.0000000
# 
# No Cached Inverse Found...Performing the Inverse...
# 
# Inverted Matrix is:
#   
#   r1         r2         r3
# [1,]  1.3636364 -0.5454545 -0.2727273
# [2,] -0.6363636  1.4545455 -0.2727273
# [3,] -0.1363636 -0.5454545  1.2272727
# 
# Time Taken:
#   user  system elapsed 
# 0.001   0.001   0.001 
# 
# Cached Inverse Found...Getting Cached Inverse...
# 
# r1         r2         r3
# [1,]  1.3636364 -0.5454545 -0.2727273
# [2,] -0.6363636  1.4545455 -0.2727273
# [3,] -0.1363636 -0.5454545  1.2272727
# 
# Time Taken:
#   user  system elapsed 
# 0.001   0.000   0.001 
# >
