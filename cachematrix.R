## Functions of project 2. Week 2. R Programming Coursera course.
## Two functions that calculate and cache the inverse of a matrix


## makeCacheMatrix(). Stores a matrix and caches its inverse.
## It has methods to control that the cached inverse corresponds with
## the stored matrix
makeCacheMatrix <- function(x = matrix()) 
{
      invers <- NULL   
      newmat <- TRUE   #TRUE for new matrix
      
      set <- function(mat = matrix())   #store a new matrix and initializes variables
      {     
            x <<- mat
            invers <<- NULL
            newmat <<- TRUE   
      }
      
      get         <-function()            {x}   
      getNewMat   <-function()            {newmat}   
      setNewMat   <-function(ch)          {newmat<<-ch}
      setInverse  <-function(inv)         {invers <<- inv}
      getInverse  <-function()            {invers}
      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse, 
           setNewMat=setNewMat, getNewMat=getNewMat)
      
}


## cacheSolve(). Computes the inverse of a matrix created by makeCacheMatrix
## If the inverse has already been calculated and the matrix hasn't changed,
##cacheSolve() returns the inverse from the cache

cacheSolve <- function(x, ...) 
{
      #Check that the matrix is square. Only square matrices are inversible.
      ncol(x$get()) != nrow(x$get())
      if (ncol(x$get()) != nrow(x$get()))   
      {
            message("The matrix must have the same number of rows and columns.
                    Please, pass a square matrix.")
            return(NULL)
      }
      #Check if inverse is already calculated
      invers <- x$getInverse()
      if (!is.null(invers) & !x$getNewMat())
      {
            message("getting cached data")
            return(invers)   #if so return cache value
      }
      #If not previously calculated, calculate inverse and cache new inverse
      data <- x$get()
      invers <- solve(data, ...)
      x$setInverse(invers)
      x$setNewMat(FALSE)
      invers
}
