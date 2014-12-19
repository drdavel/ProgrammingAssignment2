## Programming Exercise 2 DSL
## makeCacheMatrix takes and stores a matrix and inverse, caching the data
##     exposes four functions 
##       get - returns original matrix data
##       set - sets and caches new matrix data and clears inverse (NULL)
##       getinverse - returns inverse value
##       setinverse - sets and caches inverse value from matrix

makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set<-function(y){
    x<<-y
    im<<-NULL
  }
  get<-function(){x}
  setinverse<-function(inverted) {im<<-inverted}
  getinverse<-function(){im}
  list(get=get,set=set,getinverse=getinverse,setinverse=setinverse)
}


## cacheSolve takes a matrix structure from makeCache 
## and returns the inverse of that matrix and caches it.
## the cached inverse is returned if it exists

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
  im<-x$getinverse()
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  mdata<-x$get()
  im<-solve(mdata, ...)
  x$setinverse(im)
  im
}
