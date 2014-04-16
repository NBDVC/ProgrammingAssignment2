## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing makeCacheMatrix

##  This first function is kind of like an object all to itself.
##  It contains properties and methods.
##  A.The properties that this functions stores are:
##    1. The Matrix
##      This "property" can be set at the initializaiton of the object,"matrix"
##      It can also be set for an instance of the object through the set function
##    2. The Inverse of the Matrix
##      This "property" can only be set through the "class methods". Initially it is set to NULL.
##  B. Methods of this function "Class"
##    1. Return the Matrix property
##    2. Change the properties of an existing instance to native states with new Matrix information
##    3. Return the Inverse of the Matrix property
##    4. Change the Inverse of the Matrix property
##  
##  THe "properties" and "methods" of a particular instance are stored in a list of names elements.


makeCacheMatrix <- function(x = matrix()) {
  # IM: Inverse Matrix Property
  IM <- NULL                  #Initialization of new instance
  
  # This bit of code both stores the Matrix property and provides the method for its extraction A.1. & B.1.
  # get is the function "method" and x is assigned with the scope of makeCacheMatrix to the passed arguement
  # in that manner x stores the value of the matrix that was passed.
  get <- function() x
  
  # This bit ofcode allows a existing instance to be reset to a new matrix and assignes a value to a 
  # variable outside of its (set) scope.
  set <- function(y = matrix()){
    x <<- y 
    IM <<- NULL
  }
  
  # THis bit of code allows for the Inverse Matrix to be set for computation outside of this object.
  # It sets the value of IM that is a variable of its parent enviroment therefor we use "<<-"
  setIM <- function(IMx) IM <<- IMx
  
  ## This bit of code provides the method to access the Inverse matrix, Initially it is NULL as IM is set 
  ## in its parent enviroment.  When this function is called [R] looks to the scope of makeCacheVector to 
  ## get the value of IM.  That could have been modified in either method setIM or set using the <<_ assignement operator
  
  getIM <- function() IM
  
  # returns and instance of the "object"
  list(set = set, get = get, setIM = setIM, getIM = getIM)
  

}


## Write a short comment describing this function

##This function ultizes the "methods" of makeCacheMatrix to get a matrix, see if a inverse has been assigned to it
## if an inverse is assigned then it return the assigned inverse if not it computes, stores and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      IM <- x$getIM()
      if(!is.null(IM)) {
        message("returning cached Inverse Matrix")
        return(IM)
      }
      m <- x$get()
      IM<- solve(m)
      x$setIM(IM)
      IM
      
}

## Example
## source("cachematrix.R")
## m <- matrix(1:25, 5)
## for (i in 3:5) m[i,i] <- c(45,36,2)[(i-2)]
## lm<-makeCacheMatrix(m)
## IM <- cacheSolve(lm)
## m %*% IM
