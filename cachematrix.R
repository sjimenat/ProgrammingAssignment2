#matrix inversion is a costly computation
#benefits to caching the inverse of a matrix vs. compute repeatedly
#assignment: write a pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function (x=matrix()){
    inv<-NULL  #initializing inverse
    set<- function (y){
        x<<-y  #<<- operator that assigns value to an obj in an env diff from current
        inv<<-NULL
    }
    get<-function () x #use function to get the matrix and return "x", the matrix
    setInverse<-function(solveinverse) inv<<-solveinverse #setting inverse
    getInverse<-function() inv  #getting inverse
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) #list of methods
}

#compute inverse of the special matrix, "x", returned by function above
#cacheSolve should retrieve the inverse from the cache
cacheSolve<-function(x, ...){
    inv<-x$getInverse() 
    if(!is.null(inv)){  #checking if inverse has already been calculated
        message("getting cached data")
        return(inv)
    }
    data <- x$get()  #if not, retrieve inverse from the cache
    inv<-solve(data)
    x$setInverse(inv)
    inv
}
