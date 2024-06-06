### Call from GCDkit


plotDiagram.json <- function(diagram,select.samples=TRUE,new=TRUE,main=NULL,width=6.5,height=6.5,pointsize=10,
                             bg="transparent",autoscale=TRUE,interactive=FALSE,...){
  #' A replacement for GCDkit's plotDiagram, with the same API
  #'
  #' In principle, it should take the same arguments and return the same Figaro
  #' graph, so it aims to be a transparent replacement.
  #'
  #' @details
    #' Unlike GCDkit version, all diagrams are available to the user - even if
    #' the data is not there. If there is nothing to plot, the call will return an
    #' error.
    #'
    #' This function does its best to emulate \link{GCDkit::plotDiagram}.
    #'
    #' Language options are not honoured.
    #'
  #' @section{Note: } Remember that a template contains all the information to calculate
  #' point coordinates. If tt is a template, then
  #' tt$dataTransform(WR) returns the transformed data, and
  #' pointCoordinates(tt,WR,lbl) return their X and Y coordinates
  #' in this specific plot. So adding points can be done without undue effort.
  #'
  #' @param diagram Diagram to plot, here the name of a json file
  #' @param select.samples new main width height pointsize bg autoscale interactive see \link{GCDkit::plotDiagram}

  # This is a pure GCDkit function ! It should fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # From GCDkit's code, not sure why...
  if (!interactive) {
    if (!screen())
      new <- TRUE
  }

  # Select samples
  if (select.samples)
    i <- selectSamples(print = FALSE)
  else i <- rownames(WR)
  flush.console()

  # Colors
  if (!getOption("gcd.plot.bw")) {
  }
  else {
    plt.col <<- c("black", "black", "black")
  }

  # The plot proper
  tt <- parseJsonTemplate(diagram,
                                template_options = c(showText = options("gcd.plot.text") ),
                                style_options = c(pltcol1=plt.col[1],
                                                  pltcol2=plt.col[2],
                                                  pltcol3=plt.col[3]))
 if(any(class(tt)=="ternary")){
   tt <- addTernaryOrnaments(tt)
 }

  pp<-plotFigaro(tt,WR,lbl=get("labels",".GlobalEnv"))

  # Put in the global env
  assign("pp", pp, .GlobalEnv)
  assign("tt", tt, .GlobalEnv)

  # Sample identification
  if (getOption("gcd.ident"))
    ee <- ID(x.data, y.data)

  # Check range
  out <- names(which(x.data[!is.na(x.data)] < min(sheet$demo$call$xlim)))
  out <- c(out, names(which(x.data[!is.na(x.data) & x.data !=
                                     "Inf"] > max(sheet$demo$call$xlim))))
  out <- c(out, names(which(y.data[!is.na(y.data)] < min(sheet$demo$call$ylim))))
  out <- c(out, names(which(y.data[!is.na(y.data) & y.data !=
                                     "Inf"] > max(sheet$demo$call$ylim))))
  out <- unique(out)
  if (length(out) > 0) {
    cat("WARNING! - in", diagram,
        "\nsome analyses plot out of the limits of the current plot.....\n")
    ee <- cbind(x.data[out], y.data[out])
    rownames(ee) <- out
    colnames(ee) <- c("x", "y")
    ii <- apply(ee, 1, function(i) {
      all(!is.na(i))
    })
    ii <- names(ii)[ii]
    ee <- subset(ee, rownames(ee) %in% ii, drop = FALSE)
    cat("Samples not plotted:\n")
    print(ee)
    cat("\n")
    flush.console()
    if ((getOption("gcd.menus") != "" | !new) & autoscale) {
      figFixLim(no.action.warn = FALSE)
    }
    else {
      xx <- paste(round(sheet$demo$call$xlim, 2), collapse = " to ")
      yy <- paste(round(sheet$demo$call$ylim, 2), collapse = " to ")
      cat("Actual plot ranges for x-axis: [", xx, "] and for y-axis: [",
          yy, "]\n", sep = "")
      xx <- paste(round(range(x.data[x.data != "Inf"],
                              na.rm = TRUE), 2), collapse = " to ")
      yy <- paste(round(range(y.data[y.data != "Inf"],
                              na.rm = TRUE), 2), collapse = " to ")
      cat("Actual data ranges for x-axis: [", xx, "] and for y-axis: [",
          yy, "]\n", sep = "")
      flush.console()
    }
  }

  if (!new) {
    options(show.error.messages = FALSE)
    print(.Device)
    ee <- screen()
    .saveCurPlotDef(ee)
    scr.old <<- ee
    if (ee < length(plate.data))
      screen(ee + 1, new = FALSE)
  }
  else {
    if (getOption("gcd.menus") != "") {
      figaroOn()
    }
  }
  invisible(pp)
}


addData <- function(tpl,newData,col,pch){
  #' A simple function demonstrating how to use the template info to add points
  #' to a graph...
  #'
  #' @param tpl a graphTemplate (of this class)
  #' @param newData The new compositions, in WR style
  #' @param col, pch: symbols and colors
  #'
  #' @details This is a skeleton function, mostly for the purpose of demonstrating
  #' that the template does store some interesting information...

  # Create a "mock" labels
  lab <- data.frame(col=rep(col,nrow(newData)),
               pch=rep(pch,nrow(newData)))

  # Get plotting coordinates
  newpoints <- pointCoordinates(tpl,newData,lab)

  # Add the points to the graph (assuming it is still there...)
  points(newpoints$plottingCoords[,"x.data"],
         newpoints$plottingCoords[,"y.data"],
         col=newpoints$lbl[,"col"],
         pch=newpoints$lbl[,"pch"])

  }


addLine<-function(xl,yl,lty="solid",lwd=1,col=plt.col[1],redraw=T){
  #' A simple function demonstrating how to use the template info to add lines
  #' to a graph template
  #'
  #' @param xl xl line coordinates
  #' @param col, lty, lwd: of the line
  #' @param redraw probably yes...
  #'
  #' @details This is a skeleton function, mostly for the purpose of demonstrating
  #' that the template does store some interesting information...
  #' Since it is in the template however, it will survice resizing, multiple
  #' plots by groups and so on.

  origsheet <- sheet
  my <- list(my = list(
    "lines",
    x = xl,
    y = yl,
    lty=lty,
    lwd=lwd,
    col=col
  ))

  ee <- c(origsheet$demo$template, my)
  sheet <- origsheet
  sheet$demo$template <- ee

  # Put things back the way they were
  assign("sheet", sheet, .GlobalEnv)

  # Redraw, etc.
  if (redraw) {
    figRedraw()
  }

  figaroOn()
}

