### Ternary coordinates ####
#' Calculate ternary coordinates based on three variables, A, B and C
#' @param A,B,C numeric vectors the values
#' @param rotation rotation angle, degrees
#' @param size size of the triangle
#' @returns a five-colum data.frame : X, Y, and the A, B and C coordinates (as supplied).
ternaryCoordinates <- function(A,B,C,rotation=0,size=1){
  sum_apices <- A+B+C

  x.data <- ((C/sum_apices) + (B / sum_apices) /2) * size
  y.data <- (sqrt(3)*(B / sum_apices)/2) * size

  return(cbind(x.data,y.data,a.data=A,b.data=B,c.data=C))
}


#' ### Ternary axes ###
#' ternaryAxes <- function(rotation=0,size=1){
#' #' WiP
#' }
