# Function with total of two variables: x and z
# X is a numeric vector
# z for storing the results

makeCacheMatrix <- function(x = numeric()) {
  z <- NULL  
  set <- function(y) { # Sets a matrix associated with the object created by the function
    x <<- y
    z <<- NULL
  }
  
  get <- function() x  # Function that returns x
  setmat <- function(mean) z <<- mean  # Value of the mean assigned to z
  getmat <- function() z # Return the function results
  # makeCacheMatrix function returns a list with four elements,
  # Each element is a function defined earlier
  list(set = set,
       get = get,
       setmat = setmat,
       getmat = getmat) 
}

cacheSolve <- function(x, ...) {
  z <- x$getmat() # Inversed matrix of x
  if(!is.null(z)) { # z not NULL if calculated
    message("getting cached data")
    return(z) # Return the inversion's results
  }
  data <- x$get() # To be performed if there is no calculation done before
  z <- solve(data, ...) # Calculate the mean
  x$setmat(z) # Set it to the object
  z # Return the result
}

# Random square matrix
test <- matrix(runif(9,5,50),3,3)
# Run the function
inverse <- makeCacheMatrix(test)
# Next the result will be cashed
test.mat <- cacheSolve(inverse)
test.mat <- cacheSolve(inverse)
test.mat
