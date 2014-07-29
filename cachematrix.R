## Example of Lexical Scoping in R.
## The two functions save the variable (matrix, vector etc)
## using <<- operator, which can be used to store data outside
## definig environment. It can be useful when doing repeated
## costly computations in the function
## These functions actually help store inverse of matrix 

##In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current #environment. Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#	#1.	set the value of the vector
#	#2.	get the value of the vector
#	#3.	set the value of the mean
#	#4.	get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
	#initialize the inverse to NULL
	inverse <- NULL
	#Define a new function to set x
	set <- function(value){
		#store the value in x
		#assign argument to x variable with <<- operator
		x <<- value
		#set inverse as this is not value
		inverse <<- NULL
	}
	
	#Define a new function to get the value of x
	get <- function(){
		x
	}
	
	#Similarly define function to set the inverse of matrix
	setInverse <- function(i){
		#assign argument to inverse variable with <<- operator
		inverse <<- i
	}
	
	#Similarly define function to get value of inveres of matrix
	getInverse <-function(){
		inverse
	}
	
	#Return list of these functions defined under the main function
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function we find inverse of matrix and use above
## function to store and retrieve it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        
        ## If the inverse of matrix is already set, return it
        if(!is.null(inverseMatrix)){
        		return(inverseMatrix)
        }
        
        #
        data<-x$get()
        ## Calculate the inverse of a matrix using matrix multiplication
        inverseMatrix <- solve(data) %*% data
        
        ## Call setinverse method to save matrix
        x$setInverse(inverseMatrix)
        
        ## Return the required matrix
        inverseMatrix
}
