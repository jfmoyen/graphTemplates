#### Generic
#' plotFigaro
#'
#' Plot a template in Figaro context
#' @rdname plotFigaro.graphTemplate
#' @export
plotFigaro <- function(self,wrdata,lbl,new,...) {
  UseMethod("plotFigaro")
}


### This is to avoid check() notes when using global variables
# or unquoted arguments in dplyr/ggplot
utils::globalVariables(c("demo","sheet","x.data","y.data"))

##NB There is deliberately no import from GCDkit to avoid creating a dependency !

plotFigaro.graphTemplate <- function(self,wrdata,lbl,new=F,...){
  #' S3 method to plot a template, using Figaro
  #'
  #' @details Figaro is the internal plotting system of GCDkit.
  #' This method translated a template object, of class graphTemplate, into
  #' a Figaro template (very similar) and invokes Figaro's plotting function.
  #'
  #' Figaro is not written using classes (so there is no Figaro class). Also, it operates
  #' on global variables (x.data, y.data, sheet...), so expect LOTS of side effects
  #' when using this method. Of course, the most visible side-effect is plotting
  #' a new graph...
  #'
  #' Obviously, this will not work if GCDkit is not present...
  #'
  #' Different methods are supplied for binary and ternary graphTemplate.
  #'
  #' @param self a graphTemplate object
  #' @param wrdata a MATRIX containing WR analyses, as per GCDkit's convention.
  #' Probably WR (a global variable) in GCDkit context.
  #' @param lbl a data.frame containing labels, as per GCDkit's convention.
  #' Probably labels (a global variable) in GCDkit context.
  #' @param new boolean. If true, open a new graphic window.
  #' @param ... just in case
  #'
  #' @export



  # This is a pure GCDkit function ! It should fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this function.",
      call. = FALSE
    )
  }
   return(self)
}

plotFigaro.binary <- function(self,wrdata,lbl,new=F,...){
  #' @export
  #' @rdname plotFigaro.graphTemplate

  self <- NextMethod()

  # Get the X and Y values
  cc <- pointCoordinates(self,wrdata,lbl)
  x.data <- cc$plottingCoords[,"x.data"]
  y.data <- cc$plottingCoords[,"y.data"]

  ## Axes preparation
  # Custom axes names
  xlab <- makeName(self,"X")
  ylab <- makeName(self,"Y")

  # Log scales
  if(is.null(self$log)){
    which.log <- ""
  }else{
    which.log <- self$log
  }

  # Suppress axes
  if(is.null(self$suppressAxes) || !self$suppressAxes){
    axes <- TRUE
  }else{
    axes <- FALSE
  }

  #### Build the figaro "style sheet" ####
  sheet<-list(demo=list(fun="plot",
                        call=list(xlim = self$limits$X,
                                  ylim = self$limits$Y,
                                  xlab=xlab,
                                  ylab=ylab,
                                  log=which.log,
                                  bg="transparent",
                                  fg="black",
                                  xaxs = "i", yaxs = "i",
                                  #asp=1,
                                  axes=axes,
                                  new=new),
                        template=self$template))

  # Assign to global env
  # Yes this is ugly but if you want Figaro, you have it :-)
  assign("sheet", sheet, .GlobalEnv)
  assign("x.data", x.data, .GlobalEnv)
  assign("y.data", y.data, .GlobalEnv)

  #### Create the actual figaro object and plot ####
  pp <- GCDkit::figaro(demo, prefix = "sheet")

  pp$draw(x.data, y.data,
          main=GCDkit::annotate(self$fullName),
          xlab=xlab,
          ylab=ylab,
          col=cc$lbl$Colour,
          pch=cc$lbl$Symbol,
          cex=cc$lbl$Size,
          plotting.function = "fromJSON",
          new = new
  )

  invisible(pp)

} ##end

plotFigaro.ternary <- function(self,wrdata,lbl,new=F,...){
  #' @export
  #' @rdname plotFigaro.graphTemplate

  self <- NextMethod()

  # Get the X and Y values
  cc <- pointCoordinates(self,wrdata,lbl)
  x.data <- cc$plottingCoords[,"x.data"]
  y.data <- cc$plottingCoords[,"y.data"]

  ## Axes preparation
  # Custom axes names
  alab <- makeName(self,"A")
  blab <- makeName(self,"B")
  clab <- makeName(self,"C")

  # Labels
  # TODO scale and rotation !
  A=list(type="text",x=0,y=-0.03,text=GCDkit::annotate(alab),adj=0.5)
  B=list(type="text",x=0.5,y=sqrt(3)/2+.03,text=GCDkit::annotate(blab),adj=0.5)
  C=list(type="text",x=1,y=-0.03,text=GCDkit::annotate(clab),adj=0.5)

  # Include in template
  self$template <- c(self$template,A=list(A),B=list(B),C=list(C))

  # Draw pseudo axes
  self <- addTernaryAxes(self)

  #### Build the figaro "style sheet" ####
  sheet<-list(demo=list(fun="plot",
                        call=list(xlim = self$limits$X,
                                  ylim = self$limits$Y,
                                  xlab=NULL,
                                  ylab=NULL,
                                  log="",
                                  bg="transparent",
                                  fg="black",
                                  xaxs = "i", yaxs = "i",
                                  asp=1,
                                  axes=FALSE,
                                  new=new),
                        template=self$template ))

  # Assign to global env
  assign("sheet", sheet, .GlobalEnv)
  assign("x.data", x.data, .GlobalEnv)
  assign("y.data", y.data, .GlobalEnv)

  #### Create the actual figaro object and plot ####
  pp <- GCDkit::figaro(demo, prefix = "sheet")

  pp$draw(x.data, y.data,
          main=GCDkit::annotate(self$fullName),
          xlab=NULL,
          ylab=NULL,
          col=cc$lbl$Colour,
          pch=cc$lbl$Symbol,
          cex=cc$lbl$Size,
          plotting.function = "fromJSON",
          new = new
  )

  invisible(pp)

}

#### Generic
pointCoordinates <- function(self,wrdata,lbl) {
  #' @rdname pointCoordinates.graphTemplate
  #' @param self a template
  #' @export
  UseMethod("pointCoordinates")
}


pointCoordinates.graphTemplate <- function(self,wrdata,lbl){
  #' Calculate the plotting coordinates of data points in a graphTemplate
  #'
  #' @details Graph templates include the definitons of axes and, if appropriate,
  #' some transformation; for instance, data can be first transformed (e.g. CIPW norm)
  #' and the plotted in a ternary diagram, with x and y being functions of the apices.
  #' This function returns the plotting coordinates of its argument.
  #'
  #' Some graph templates also require certain conditions to be fullfilled, for
  #' instance plotting only samples with SiO2 < 54 %. Filtering is also done here based
  #' on the template.
  #'
  #' A filtered versions of the labels is also returned, preserving the information
  #' (notably col and pch) only for the samples that are "allowed" in this diagram.
  #' If you don't want data filtering, the easiest is to edit the template before plotting:
  #' self$filtering <- NULL.
  #'
  #' Filtering is done using GCDkit's selectSubset, and coordinates come from
  #' GCDkit::calcCore.
  #'
  #' pointCoordinates is called by plotFigaro, but it can also be used on it own to
  #' add points to an existing graph...
  #'
  #' @param self A graphTemplate
  #' @param wrdata a MATRIX containing WR analyses, as per GCDkit's convention.
  #' Probably WR (a global variable) in GCDkit context.
  #' @param lbl a data.frame containing labels, as per GCDkit's convention.
  #' Probably labels (a global variable) in GCDkit context.
  #' @returns A list with two components, plottingCoords (two-columns data.frame, x and y)
  #' and lbl (labels, filtered from the input data).
  #' @export

  msg <- paste("Sorry, cannot work on graph of type",class(self)[1],"\n",sep=" ")
  stop(msg)

}

pointCoordinates.binary <- function(self,wrdata,lbl){
  #' @export
  #' @rdname pointCoordinates.graphTemplate
  newdata <- cbind(self$dataTransform(wrdata),wrdata)

  # Filter, if needed
  if(!is.null(self$dataFilter)){
    selected <- GCDkit::selectSubset(what=self$dataFilter,
                                     where=cbind(lbl,newdata),
                                     all.nomatch=F,
                                     save=F)
    if(selected==""){stop("No data to plot matching criteria")}
    newdata <- newdata[selected,,drop=F]
    lbl <- lbl[selected,,drop=F]
  }


  x.data <- GCDkit::calcCore(self$axesDefinition$X,where="newdata",redo=F)$results
  y.data <- GCDkit::calcCore(self$axesDefinition$Y,where="newdata",redo=F)$results

  return(list(plottingCoords=cbind(x.data,y.data),
              lbl=lbl))
}

pointCoordinates.ternary <- function(self,wrdata,lbl){
  #' @export
  #' @rdname pointCoordinates.graphTemplate

  newdata <- cbind(self$dataTransform(wrdata),wrdata)

  # Filter, if needed
  if(!is.null(self$dataFilter)){
    selected <- GCDkit::selectSubset(what=self$dataFilter,
                                     where=cbind(lbl,newdata),
                                     all.nomatch=F,
                                     save=F)
    if(selected==""){stop("No data to plot matching criteria")}
    newdata <- newdata[selected,,drop=F]
    lbl <- lbl[selected,,drop=F]
  }

  a.data <- GCDkit::calcCore(self$axesDefinition$A,where="newdata",redo=F)$results
  b.data <- GCDkit::calcCore(self$axesDefinition$B,where="newdata",redo=F)$results
  c.data <- GCDkit::calcCore(self$axesDefinition$C,where="newdata",redo=F)$results

  return(list(plottingCoords=ternaryCoordinates(a.data,b.data,c.data),
              lbl=lbl))
}



#### Generic
#' makeName
#'
#' Nice looking name for an axis
#'
#' @param self a template
#' @param W the axis to format
# In fact the problem is worse. In a non GCDkit context we can
# not use GCDkit::annotate. The axes are defined as variables
# which allows to use them in the data. So we must find either
# (i) a non-GCDkit way to do the equivalent of annotate or
# (ii) perhaps easier, override axes labels somehow (and manually).
#' @export
makeName <- function(self,W) {
  UseMethod("makeName")
}


makeName.graphTemplate <- function(self,W){
  #' @export
  #' @rdname makeName

  if(!is.null(self$axesName[[W]])){
    wlab <- GCDkit::annotate(self$axesName[[W]])
  }else{
    wlab <- GCDkit::annotate(self$axesDefinition[[W]])
  }

}



