## Put comments here that give an overall description of what your
## functions do
## dimension calculates the dimension of the matrix
## matriz creates the matrix to work with
## inversa uses solve function to calculate the inverse of "matriz" #
## Write a short comment describing this function: calculates the dimension of the matrix and then calculates its inverse if it has not been calculated before#

makeCacheMatrix<-function(x=numeric()){
  dimension<-sqrt(length(x))
  matriz<-matrix(x,dimension,dimension)
  inversa<-NULL
  elem<-function() matriz
  valinv<-function(solve) inversa <<- solve
  calcinv<-function() inversa
  list(elem=elem, valinv=valinv, calcinv=calcinv)
}
cacheSolve<-function(x,...) {
  inversa<-x$calcinv()
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  info<-x$elem()
  inversa<- solve(info)
  x$valinv(inversa)
  inversa
}
