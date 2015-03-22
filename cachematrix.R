## Put comments here that give an overall description of what your
#makeCacheMatrix takes in a matrix creates an cachable matrix object
#the item returned has 4 functins as described below

#the cachesolve takes in a matrix object, checks if it has a cached inverse and either
#returns taht value or calcuates, caches and returns the inverse

## Write a short comment describing this function
#this takes a matrix and returns an list with 4 functions
	#we return a list containng:
	#1 a set function
	#2 a get function
	#3 a get inverse function
	#4 a set inverser function

makeCacheMatrix <- function(x = matrix()) {
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



	list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## Write a short comment describing this function
      ## Return a matrix that is the inverse of 'x'
	#cache solve is designed to return an inverted matrix to the user
	#the function uses the cached matrix object to return either
	#the cached inverse or the newly calculated inverse

cacheSolve <- function(x, ...) {

	tInv<-x$getInverse()
	
	if(!is.null(tInv)){	#if the inverse of x exists in cache (isn't null)
		#the cache exists, return it
		message("Cached")
		return(x$getInverse())
	}else{
	#else, the cache does not exist and we need to get it
		tMat<-x$get()		#get the matrix
		nInv<-FALSE
		tryCatch({
			iMat<<-solve(x$get())
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
