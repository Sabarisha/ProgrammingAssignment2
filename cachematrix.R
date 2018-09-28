## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Assuming matrix I pass is invertible matrix
makeCacheMatrix <- function(x = matrix()) {
        #Creating an empty matrix to store the inverted matrix
        invmat<-matrix()
        getmat<-function() x
        setinv<-function(inv=matrix()) invmat<<-inv
        getinv<-function() invmat
        list(getmat=getmat,setinv=setinv,getinv=getinv)
}

## My makeCacheMatrix will return a list of functions which will get original matrix passed and empty values for other columns 
##if a new matrix is passed everytime
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat<-x$getinv()
        if(all(!is.na(invmat)))
                {
                message("getting cached data")
                return(invmat)
                }
        data<-x$getmat()
        invmat<-solve(data,...)
        x$setinv(invmat)
        invmat    
        }
