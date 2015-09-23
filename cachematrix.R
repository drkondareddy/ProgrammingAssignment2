## Overall this program can be used to put a matrix into cache
## and retrieve the cache if the same kind of inverse calculation
## is performed.

## This function accepts the matrix as input. 
## Calculates the inverse of the matrix.
## the result matrix will be stored into the Cache.

makeCacheMatrix <- function(x = matrix()) {

	## m stores the cached value. initializes to NULL.		

  				m<-NULL

	## x stores the values in working env.

  				set<-function(y){
  					x<<-y
  					m<<-NULL
				}

	## get will get the value of matrix.			

		get<-function() x

	## inverse the matrix and store in cache.

		setmatrix<-function(inverse) m<<- inverse

	## get the resulting inverted matrix from cache.

		getmatrix<-function() m

	## create a list of all the function variables for current work

		list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

## CacheSolve will take the matrix as input. 
## check to see if the inverse is available in cache.
## If it is available in Cache then it returns the resulting matrix
## OTherwise it calculates the inverse and returns the resulting matrix

cacheSolve <- function(x=matrix(), ...) {

	## get the inverted matrix from cache.

    		m<-x$getmatrix

	## return the inverted matrix if it exists in cache.

    		if(!is.null(m)){
      		message("getting cached data")
     		 	return(m)
    		}

	## matrix will be created if it doesnt exist in cache.

    		matrix<-x$get

	## inverse the matrix

    		m<-solve(matrix, ...)

	## inverted matrix

    		x$setmatrix(m)

	## return the resulting matrix
    	return(m)
    	}
