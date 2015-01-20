## Use the following command to test
## cacheSolve(makeCacheMatrix(matrix(c(1:4), nrow =2, ncol =2)))
## There are two functions which have been used for this assignment
## 1. makeCacheMatrix
## 2. cacheSolve
## makeCacheMatrix function receives a matrix() as an input and stores it into cache. It also stores the output into cache. 
## cacheSolve fuction uses makeCacheMatrix return functions to store the input and putput matrixes to cache. 

## Accepts matrix as input and caches input and output matrix 
## Returns functions being used to cache the matrixes
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the output matrix variable
  m<-NULL
  ## assign value to input cache matrix variable
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## return the input matrix variable
  get<-function(){
    return(x)
  } 
  ## assign value to output cache matrix variable 
  setmatrix<-function(solve) {
    m<<- solve
  }
  ## return output cache matrix variable
  getmatrix<-function() {
    return(m)
  }
  ## return the list of functions that can be performed on input variable
  return(list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix))
}

## accepts list of functions as input and uses them to cache the input & output matrix 
cacheSolve <- function(x, ...) {
  ## initialize the output matrix 
  m<-x$getmatrix()
  ## if not initialized means it has some cached data
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## assign value to input matrix
  matrix<-x$get()
  ## Verify the number of rows and determinant to check inversible or not
  ## Determinant of a matrix deceides if it is inversible. It should not be zeroes.
  ## e.g. matrix(c(1:9), nrow =3, ncol =3) is having determinant as zeroes
  if ((nrow(matrix) != ncol(matrix)) | (det(matrix) == 0))  {
    message("invalid matrix")
    return(m)
  }
  ## inverse the input matrix using solve function
  m<-solve(matrix, ...)
  ## assign value to output cache matrix
  x$setmatrix(m)
  ## return output cache matrix
  return(m)
}
