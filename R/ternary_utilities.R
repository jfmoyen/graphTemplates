### Ternary coordinates ####

ternaryCoordinates <- function(A,B,C,rotation=0,scale=1){
  #' Calculate ternary coordinates based on three variables, A, B and C
  #' @param A,B,C numeric vectors the values
  #' @param rotation rotation angle, degrees
  #' @param scale size of the triangle
  #' @returns a five-column data.frame : X, Y, and the A, B and C coordinates (as supplied).
  #'
  #' @export

  sum_apices <- A+B+C

  x.data <- ((C/sum_apices) + (B / sum_apices) /2) * scale
  y.data <- (sqrt(3)*(B / sum_apices)/2) * scale

    rr <- rotateCoordinates(x.data,y.data,rotation,scale)

  return(cbind(x.data=rr[,"Xr"],y.data=rr[,"Yr"],a.data=A,b.data=B,c.data=C))
}

rotateCoordinates <- function(X,Y,rotation=0,scale=1){
  #' Calculate coordinates following a rotation
  #'
  #' @param X,Y Original coordinates
  #' @param rotation Angle, in degree
  #' @param scale Scale factor
  #'
  #' @returns data.frame with new X and Y (Xr, Yr)
  #' @export

  Xr <- ( X * cospi(rotation / 180) - Y * sinpi(rotation / 180) ) * scale
  Yr <- ( X * sinpi(rotation / 180) + Y * cospi(rotation / 180) ) * scale

  return(cbind(Xr,Yr))
}

ternaryBoundingBox <- function(rotation,scale,padding=c(0.03,0.03,0.03,0.05)){
  #' The bounding box of a ternary diagram
  #'
  #' @param rotation Angle, in degree
  #' @param scale Scale factor
  #' @param padding Extra space to add (as pct); left, top, right, botton
  #'
  #' @returns A list with two components, X and Y.
  #'
  #' @export

  Xangles <- c(0,0,1,1)
  Yangles <- c(0,1,1,0)

  rr <- rotateCoordinates(Xangles,Yangles,rotation=rotation,scale=scale)

  bb <- list(X = c(min(rr[,"Xr"]) - padding[1], max(rr[,"Xr"]) + padding[3] ) * scale,
             Y = c(min(rr[,"Yr"]) - padding[4], max(rr[,"Yr"]) + padding[2] ) * scale )

  return(bb)
  }

#' ### Ternary axes ###
#' ternaryAxes <- function(rotation=0,size=1){
#' #' WiP
#' }

#pseudoAxes=list(type="lines",x=c(0,1,0.5,0),y=c(0,0,sqrt(3)/2,0),col="black")


