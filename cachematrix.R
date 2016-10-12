## This Document contains two functions: "makeCacheMatrix" and "cacheSolve" 
## 
## "makeCacheMatrix": 
##      creates a set of functions and returns those functions 
##      along with two objects: x and IM to be used by the "cacheSolve" function. 
##      the key function corresponds to invert a square matrix. 
##      The function checks for the class(is.matrix) of the input (x), and notifies the user 
##      if the original input (x) is not a matrix. 
##       Similarly, if the matrix is not square (dimensions are not similar) also send the message 
##      that the user should modify the input (x) before attempting to run the following function   


makeCacheMatrix <- function(x = matrix()) {
        ## checking that input is an square matrix
   if (!is.matrix(x)) {
                print("ERROR: Your input is not a matrix... Please Try again")
                print("x =")
                print(x)
                return()
        }
        else {
                if(dim(x)[1]!=dim(x)[2]) {
                        print("ERROR:Your matrix is not square, so no inverse is possible...Please try again")
                        print("x =")
                        print(x)
                        return()
                }
        }
    
        IM <- NULL
        set <- function(y) {
                x <<- y
             
                IM <<- NULL
        }
        get <- function() x
        setIM <- function(solve) IM <<- solve
        getIM <- function() IM
        list(set = set, get = get,
             setIM = setIM,
             getIM = getIM)
       
        
}
        

## "cacheSolve": 
##      This function needs the set of functions (X) and objects created by "makeCacheMatrix" 
##      By using the functions and objects, it first checks if there is already an inverse 
##      solution for the matrix input. If a solution is not already available, then obtains 
##      the inverse of the matrix, store it and presented. If a subsequent evaluation of the 
##       function is performed, and the set of functions (X) has not been modified, then, the 
##      function prints a statement indicating that the inverse is already available, and 
##      extract it from the cache and print it.  
##      BIGNOTE: 
##        ***If the Matrix is Singular, then an ERROR will occur while running this function.***
##      NO ATTEMPT OF ACCOUNTING FOR THIS ERROR WERE COEDED.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        IM <- x$getIM()
         if(!is.null(IM)) {
                message("getting cached data")
         }
        data <- x$get()
        IM <- solve(data)
        x$setIM(IM)
        IM
        
}
