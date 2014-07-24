##this function is used to cache the matrix.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() {x}
  setmatrix<-function(x) {m<<- x}
  getmatrix<-function(){ m }
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
## this function is used to create the inverse of the matrix
## if matrix is already created then it is retreived from the cache

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Getting Cached Matrix")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}