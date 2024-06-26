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
utils::globalVariables(c("WR","labels","demo","sheet","x.data","y.data","plate","plate.data","scr.old"))

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
  #' Plates are yet another animal. GCDkit does a lot of things behind the scenes
  #' when plotting a plate. Some of the arguments can be passed to the undrlying function
  #' (via ...); the interested are aventurous user will note that the function
  #' called to setup plates is .plateSetup = function (number, nrow = NULL, ncol = NULL,
  #' title = NULL, new = TRUE, device = "windows", filename = NULL, colormodel = "rgb").
  #' In RStudio, \code{device=""} can be used to send the plate to the internal viewer.
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

  cat("No plotting methods for ",class(self)[1],"implemented yet. Quitting." )

   return()
}

plotFigaro.binary <- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),new=F,...){
  #' @export
  #' @rdname plotFigaro.graphTemplate

  # This is a pure GCDkit function ! It should fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Execute hook function
  ee <- self$hook(self,wrdata,lbl)

  self <- ee$self
  wrdata <- ee$wrdata
  lbl <- ee$lbl

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

  ## Build the figaro "style sheet"
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

  ## Create the actual figaro object and plot
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

plotFigaro.ternary <- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),new=F,...){
  #' @export
  #' @rdname plotFigaro.graphTemplate

  # This is a pure GCDkit function ! It should fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Execute hook function
  ee <- self$hook(self,wrdata,lbl)

  self <- ee$self
  wrdata <- ee$wrdata
  lbl <- ee$lbl

  # Get the X and Y values
  cc <- pointCoordinates(self,wrdata,lbl)
  x.data <- cc$plottingCoords[,"x.data"]
  y.data <- cc$plottingCoords[,"y.data"]

  ## Build the figaro "style sheet"
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

  ## Create the actual figaro object and plot
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

plotFigaro.plate <- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),new=F,...){
  #' @export
  #' @rdname plotFigaro.graphTemplate
  #' @importFrom graphics mtext screen par

  # This is a pure GCDkit function ! It should fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this function.",
      call. = FALSE
    )
  }

  ## Create the plate itself
  plate <- GCDkit::.plateSetup(self$nbslots, self$nrow, self$ncol,
                               title = self$fullName,
                               ...)

  ## Prepare the data structure, empty so far
  plate.data <- as.list(1:self$nbslots)
  plate.data <- lapply(1:self$nbslots, function(i) {
    plate.data[[i]] <- list(x = 1, y = 1)
  })
  names(plate.data) <- paste("Fig", 1:self$nbslots, sep = "")

  ## Make global
  assign("plate", plate, .GlobalEnv)
  assign("plate.data", plate.data, .GlobalEnv)

  ## Graphic setup and title
  graphics::par(oma = c(0, 0, 4, 0))
  graphics::mtext(text = GCDkit::annotate(plate$title), side = 3, line = 0.25,
                  outer = TRUE, cex = 1.5)

  ## Construct every individual plot
  ee <- lapply(1:self$nbslots, function(i) {
    graphics::screen(i, new = FALSE)

    ## Geometric considerations
    graphics::par(pty = "s")
    if (.Platform$OS.type == "windows" & .Platform$GUI ==
        "Rgui") {
      graphics::par(mar = c(4.5, 5.5, 2, 1.5))
    }
    else {
      graphics::par(mar = c(2, 0.5, 1, 1))
    }

    ## The actual plot
    plotFigaro(self$plateSlots[[i]],wrdata=wrdata,lbl=lbl,new=new)
    GCDkit::.saveCurPlotDef(i)
  })

  ## Final touch
  assign("scr.old", 1, .GlobalEnv)
  graphics::screen(1, new = FALSE)
  if (.Platform$OS.type == "windows" & .Platform$GUI == "Rgui")
    GCDkit::.menuPopUp()
  graphics::screen(1, new = FALSE)

  invisible(self)
}



