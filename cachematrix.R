## Perform Matrix inversion and create a cache of the inverted matrix 
## for ease of future retrieval without having to perform the inversion again

## Write a short comment describing this function
## This function create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #Initialize x
  # Initialize the inverse_matrix object that will become the cache
  inverse_matrix <- NULL 
  # Define a function that set and reset the matrix and its corresponding inverse
  # I used the <<- operator to assign the value to pbejcts in the parent environ
  set <- function(y) {
    
    x <<- y 
    inverse_matrix <- NULL 
  }
  # A function to get the current value of this special matrix
  get <- function() x
  
  # A function to set the inverse matrix cache
  set_inverse <- function(new_inverse_matrix) inverse_matrix <<- new_inverse_matrix
  
  # A function to retrieve the inverse from the cache
  get_inverse <- function() inverse_matrix
  
  # Return a list containing the functions above
  # Since the list contains functions defined with the makeCacheMatrix() environ,
  # the environment and its objects are preserved within this special matrix,
  # including the cahced inverse matrix
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This is the actual function that performs the inversion of the special matrix 
## created by the makeCacheMatrix() function and then save the inversed matrix into 
## the cached object inverse_matrix

cacheSolve <- function(x, ...) {
  # Check to see if the inverse matrix has been calculated before and cached
  inverse_matrix <- x$get_inverse()
  if(!is.null(inverse_matrix)){
    message("Getting cached data")
    return(inverse_matrix)
  }
  
  #If there is no cached matrix, then solve the inversion 
  Matrix <- x$get()
  Inverse <-solve(Matrix)
  
  # Save this intot he cache of the matrix
  x$set_inverse(Inverse)
  
  # Return the inverse
  return(Inverse)
}
