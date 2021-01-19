# Create matrix function that is able to store inverse
# Create function that pulls stored value if available, otherwise calculates it

# Function that defines matrix and stores inverse
makeCacheMatrix <- function(x=matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix_inverse) m_inv <<- matrix_inverse
  getinverse <- function() m_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function that computes inverse if not cached in makeCacheMatrix
# x argument for this function must be the makeCacheMatrix function
cacheSolve <- function(x, ...){
  m_inv <- x$getinverse()
  if(!is.null(m_inv)) {
    message("Getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinverse(m_inv)
  m_inv
}
