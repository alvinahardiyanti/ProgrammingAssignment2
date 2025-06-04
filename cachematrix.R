## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached matrix inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y      # Assign the new matrix
    inv <<- NULL # Reset the cached inverse because the matrix has changed
  }
  
  # Function to get the matrix
  get <- function() {
    x # Return the stored matrix
  }
  
  # Function to set the calculated inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse # Store the inverse in the cache
  }
  
  # Function to get the inverse of the matrix from the cache
  getInverse <- function() {
    inv # Return the inverse stored in the cache
  }
  
  # Return a list containing all the above functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Try to get the inverse from the cache
  inv <- x$getInverse()
  
  # If the inverse is already in the cache, return it
  if (!is.null(inv)) {
    message("getting cached data") # Message indicating data is retrieved from cache
    return(inv)
  }
  
  # If the inverse is not in the cache, get the matrix
  mat_data <- x$get()
  
  # Calculate the matrix inverse using the solve() function
  # Assumption: the matrix is always invertible
  inv <- solve(mat_data, ...)
  
  # Store the newly calculated inverse in the cache
  x$setInverse(inv)
  
  # Return the calculated inverse
  inv
}

# --- Example Usage ---

# 1. Create a square invertible matrix
my_matrix <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)

# 2. Create the special "matrix" object using makeCacheMatrix
cached_matrix_object <- makeCacheMatrix(my_matrix)

# 3. First call to cacheSolve: will compute the inverse
print("First call:")
print(cacheSolve(cached_matrix_object))

# 4. Second call to cacheSolve: will retrieve the inverse from cache
print("Second call:")
print(cacheSolve(cached_matrix_object))

# 5. Change the stored matrix
new_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2) # matrix [[1,3],[2,4]]
cached_matrix_object$set(new_matrix)

# 6. Call cacheSolve again: will compute the inverse for the new matrix
print("After changing the matrix:")
print(cacheSolve(cached_matrix_object))

# 7. Call cacheSolve once more: will retrieve the new inverse from cache
print("Fourth call (after change):")
print(cacheSolve(cached_matrix_object))
