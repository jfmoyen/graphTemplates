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

ternaryTicks <- function(length=0.05,interval=10,lwd=1,lty="solid",col="black"){
  #' Generate templateElements defining ticks
  #'
  #' @param interval Number of intervals on each axis. (interval-1) ticks
  #' will be drawn...
  #' @param length How far they should project in the diagram
  #' @param lwd,lty,col Usual parameters for the ticks
  #'
  #' @return a list of templateElement objects defining ticks
  #'
  #' @export

  # Root points
  ptsBA <- data.frame(A = seq(1/interval,1-1/interval,by=1/interval),
                    B = seq(1-1/interval,1/interval,by=-1/interval),
                    C = 0)
  ptsCA <- data.frame(A = seq(1/interval,1-1/interval,by=1/interval),
                    B = 0,
                    C = seq(1-1/interval,1/interval,by=-1/interval)
  )
  ptsBC <- data.frame(A = 0 ,
                    B = seq(1-1/interval,1/interval,by=-1/interval),
                    C = seq(1/interval,1-1/interval,by=1/interval)
  )
  # Projections
  ptsBA_B <- data.frame(A = ptsBA$A - length,
                        B = ptsBA$B,
                        C = length)
  ptsBA_A <- data.frame(A = ptsBA$A,
                        B = ptsBA$B - length,
                        C = length)

  ptsCA_C <- data.frame(A = ptsBA$A - length,
                        B = length,
                        C = ptsCA$C)
  ptsCA_A <- data.frame(A = ptsBA$A,
                        B = length,
                        C = ptsCA$C - length)

  ptsBC_B <- data.frame(A = length,
                        B = ptsBC$B,
                        C = ptsBC$C - length)
  ptsBC_C <- data.frame(A = length,
                        B = ptsBC$B - length,
                        C = ptsBC$C)


  ptsBA.xy <- ternaryCoordinates(ptsBA$A,ptsBA$B,ptsBA$C)
  ptsBA_A.xy <- ternaryCoordinates(ptsBA_A$A,ptsBA_A$B,ptsBA_A$C)
  ptsBA_B.xy <- ternaryCoordinates(ptsBA_B$A,ptsBA_B$B,ptsBA_B$C)

  ptsCA.xy <- ternaryCoordinates(ptsCA$A,ptsCA$B,ptsCA$C)
  ptsCA_A.xy <- ternaryCoordinates(ptsCA_A$A,ptsCA_A$B,ptsCA_A$C)
  ptsCA_C.xy <- ternaryCoordinates(ptsCA_C$A,ptsCA_C$B,ptsCA_C$C)

  ptsBC.xy <- ternaryCoordinates(ptsBC$A,ptsBC$B,ptsBC$C)
  ptsBC_B.xy <- ternaryCoordinates(ptsBC_B$A,ptsBC_B$B,ptsBC_B$C)
  ptsBC_C.xy <- ternaryCoordinates(ptsBC_C$A,ptsBC_C$B,ptsBC_C$C)

  count <- 1:(interval-1)
  # Ticks protruding from AB
  ticksBA_A<-lapply(count,
                  function(i){
                    make_line(ptFrom = ptsBA.xy[i,], ptTo = ptsBA_A.xy[i,],col=col,lty=lty,lwd=lwd)
                  })
  ticksBA_B<-lapply(count,
                    function(i){
                      make_line(ptFrom = ptsBA.xy[i,], ptTo = ptsBA_B.xy[i,],col=col,lty=lty,lwd=lwd)
                    })

  ticksCA_A<-lapply(count,
                    function(i){
                      make_line(ptFrom = ptsCA.xy[i,], ptTo = ptsCA_A.xy[i,],col=col,lty=lty,lwd=lwd)
                    })
  ticksCA_C<-lapply(count,
                    function(i){
                      make_line(ptFrom = ptsCA.xy[i,], ptTo = ptsCA_C.xy[i,],col=col,lty=lty,lwd=lwd)
                    })

  ticksBC_B<-lapply(count,
                    function(i){
                      make_line(ptFrom = ptsBC.xy[i,], ptTo = ptsBC_B.xy[i,],col=col,lty=lty,lwd=lwd)
                    })
  ticksBC_C<-lapply(count,
                    function(i){
                      make_line(ptFrom = ptsBC.xy[i,], ptTo = ptsBC_C.xy[i,],col=col,lty=lty,lwd=lwd)
                    })

  return(c(ticksBA_A,ticksBA_B,ticksCA_A,ticksCA_C,ticksBC_B,ticksBC_C))
}


ternaryGrid <- function(interval=10,lwd=0.5,lty="dotted",col="grey"){
  #' Generate templateElements defining a grid
  #'
  #' @param interval Number of intervals on each axis. (interval-1) lines
  #' will be drawn...
  #' @param lwd,lty,col Usual parameters for the grid
  #'
  #' @return a list of templateElement objects defining the grid
  #'
  #' @export

  ptsBA<-data.frame(A = seq(1/interval,1-1/interval,by=1/interval),
                    B = seq(1-1/interval,1/interval,by=-1/interval),
                    C = 0)
  ptsCA<-data.frame(A = seq(1/interval,1-1/interval,by=1/interval),
                    B = 0,
                    C = seq(1-1/interval,1/interval,by=-1/interval)
                    )
  ptsBC<-data.frame(A = 0 ,
                    B = seq(1-1/interval,1/interval,by=-1/interval),
                    C = seq(1/interval,1-1/interval,by=1/interval)
                    )

  ptsBA.xy <- ternaryCoordinates(ptsBA$A,ptsBA$B,ptsBA$C)
  ptsCA.xy <- ternaryCoordinates(ptsCA$A,ptsCA$B,ptsCA$C)
  ptsBC.xy <- ternaryCoordinates(ptsBC$A,ptsBC$B,ptsBC$C)

  count <- 1:(interval-1)
  # Lines // to AC
  linesAC<-lapply(count,
          function(i){
            make_line(ptFrom = ptsBA.xy[i,], ptTo = ptsBC.xy[i,],col=col,lty=lty,lwd=lwd)
          })

  # Lines // to AB
  linesAB<-lapply(count,
                  function(i){
                    make_line(ptFrom = ptsCA.xy[i,], ptTo = ptsBC.xy[interval-i,],col=col,lty=lty,lwd=lwd)
                  })

  # Lines // to BC
  linesBC<-lapply(count,
                  function(i){
                    make_line(ptFrom = ptsBA.xy[i,], ptTo = ptsCA.xy[i,],col=col,lty=lty,lwd=lwd)
                  })


  return(c(linesAC,linesAB,linesBC))
}

make_line<-function(ptFrom,ptTo,col,lty,lwd){
  #' Internal function. Make a templateElement line between two points.
  #' @param ptFrom,ptTo the start and end point
  #' @param lwd,lty,col Usual style parameters
  #'
  #' @return a templateElement object corresponding to a line between the points
  #'
  #' @export

  ee <- list(
    plotFun = "lines",
    x = c(ptFrom["x.data"],ptTo["x.data"]),
    y = c(ptFrom["y.data"],ptTo["y.data"]),
    col = col,
    lty=lty,
    lwd=lwd
  )
  class(ee)<- c("lines","templateElement",class(ee))

  return(ee)
}

