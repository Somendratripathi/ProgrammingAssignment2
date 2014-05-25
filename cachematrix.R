#makeCacheMatrix ##This function creates a special "matrix" object that can cache its inverse
library("MASS")
makeCacheMatrix<- function(x = matrix())
{
inv <- 0    #flag set in-case inverse already calculated
set <- function (mat) {
x<<- mat
inv <<- 0 }
  get <- function() x
setinverse<- function(y)
{
imat <<- y
inv <<-1
}
getinverseflag<- function(){ inv
}
getinversemat<- function(){ imat
}
list(set = set, get = get,
             setinverse = setinverse,
             getinverseflag = getinverseflag,
			 getinversemat=getinversemat)
}



#cacheSolve

cacheSolve <- function(x, ...) {    # Returns cached matrix inverse using previously computed matrix inverse
        m <- x$getinverseflag()
        if(m==1) { #checking in case inverse already calculated
                message("getting cached data")
                			imat<-x$getinversemat()
							return(imat)
        }
        matrix <- x$get()
        imat <- ginv(matrix)    #using gniv in library("Mass") which calculates the inverse of matrix 
        x$setinverse(imat)
        imat
}


