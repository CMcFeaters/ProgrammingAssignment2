## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	#there is an issue solving a 3x3 matrix.  it says it must be numeric.  need to figure oout

	#inverse flag is set to NULL upon creation, we haven't calculated it yet
	inv<-NULL	

	set<-function(nMat){
	#this function sets the value of the original Matrix to
	#whatever the new Matrix included is
		x<<-nMat
		inv<<-NULL	#reset upon recalc of matrix
	}

	get<-function() x		#return the original Matrix

	getInverse<-function() inv #return the inverse if created

	setInverse<-function(solve){ #all the "solve" means is this functions expects an instance of solve
		inv<<-solve
	}	

	#we return a list containng:
	#1 a set function
	#2 a get function
	#3 a get inverse function
	#4 a set inverser function

	list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	#cache solve is designed to return an inverted matrix to the user
	#the function uses the cached matrix object to return either
	#the cached inverse or the newly calculated inverse
	tInv<-x$getInverse()
	
	if(!is.null(tInv)){	#if the inverse of x exists in cache (isn't null)
		#the cache exists, return it
		message("In THE MUTHAFUCKIN CACHE")
		return(x$getInverse())
	}else{
	#else, the cache does not exist and we need to get it
		tMat<-x$get()		#get the matrix
		nInv<-FALSE
		tryCatch({
			iMat<<-solve(x)
			},error=function(e){
			nInv<<-TRUE
			})

 		if(nInv==TRUE){
		message("No Inverse Exists")
		iMat<<-NULL
		}
		
		x$setInverse(iMat)
		x$getInverse()				#return the inverse	
	}

}
