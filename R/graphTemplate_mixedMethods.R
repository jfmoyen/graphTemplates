
#### Generic
pointCoordinates <- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),mode="GCDkit") {
  #' @export
  #' @rdname pointCoordinates.graphTemplate
  UseMethod("pointCoordinates")
}


pointCoordinates.graphTemplate <- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),mode="GCDkit"){
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
  #' @param mode Either "GCDkit" or "ggplot". How to calculate the new coordinates -
  #' in GCDkit context use calcCore, in ggplot use mutate.
  #' @returns A list with two components, plottingCoords (two-columns data.frame, x and y)
  #' and lbl (labels, filtered from the input data).
  #' @export

  msg <- paste("Sorry, cannot work on graph of type",class(self)[1],"\n",sep=" ")
  stop(msg)

}

pointCoordinates.binary <- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),mode="GCDkit"){
  #' @export
  #' @rdname pointCoordinates.graphTemplate

  newdata <- cbind(self$dataTransform(wrdata),wrdata)
  wr_nm <- unique(colnames(newdata))
  newdata <- newdata[,wr_nm]

  if(mode == "GCDkit"){
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

      # Remove back ticks
      axX <- gsub(pattern = "`", replacement = "", self$axesDefinition$X)
      axY <- gsub(pattern = "`", replacement = "", self$axesDefinition$Y)

      # go
      x.data <- GCDkit::calcCore(axX,where="newdata",redo=F)$results
      y.data <- GCDkit::calcCore(axY,where="newdata",redo=F)$results

      # Results
      plottingCoords<- cbind(x.data,y.data,newdata)
  }

  if(mode == "ggplot"){

    # Merge all in a tibble, preserving info
    lbl_nm <- colnames(lbl)

    WRD <- GCDkitToTibble(newdata,lbl)

    # String to expression, for ggplot data-masking
    xx <- rlang::parse_expr(self$axesDefinition$X)
    yy <- rlang::parse_expr(self$axesDefinition$Y)

    # filter data
    if(!is.null(self$dataFilter)){
      df <- rlang::parse_expr(self$dataFilter)
      WRD <- dplyr::filter(WRD,!!df)
    }

    # parse
    WRD %>% dplyr::mutate(x.data = !!xx, y.data = !!yy) %>%
      {.} -> WRD

    x.data <- WRD$x.data
    y.data <- WRD$y.data

    # Split back
    lbl <- data.frame(WRD[,lbl_nm])
    row.names(lbl) <- WRD$ID_x

    # Results
    plottingCoords<- cbind(x.data,y.data,WRD[,wr_nm])
    row.names(plottingCoords) <- WRD$ID_x

    }

  return(list(plottingCoords=plottingCoords,
              lbl=lbl))
}

pointCoordinates.ternary <- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),mode="GCDkit"){
  #' @export
  #' @rdname pointCoordinates.graphTemplate


  newdata <- cbind(self$dataTransform(wrdata),wrdata)
  wr_nm <- unique(colnames(newdata))
  newdata <- newdata[,wr_nm]

  if(mode == "GCDkit"){
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

  # Remove back ticks
  axA <- gsub(pattern = "`", replacement = "", self$axesDefinition$A)
  axB <- gsub(pattern = "`", replacement = "", self$axesDefinition$B)
  axC <- gsub(pattern = "`", replacement = "", self$axesDefinition$C)

  # go
  a.data <- GCDkit::calcCore(axA,where="newdata",redo=F)$results
  b.data <- GCDkit::calcCore(axB,where="newdata",redo=F)$results
  c.data <- GCDkit::calcCore(axC,where="newdata",redo=F)$results
  # Results
  plottingCoords<- cbind(ternaryCoordinates(a.data,b.data,c.data,
                                      self$ternaryRotation,self$ternaryScale),newdata)
  }

  if(mode == "ggplot"){

    # Merge all in a tibble, preserving info
    lbl_nm <- colnames(lbl)

    WRD <- GCDkitToTibble(newdata,lbl)

    # String to expression, for ggplot data-masking
    aa <- rlang::parse_expr(self$axesDefinition$A)
    bb <- rlang::parse_expr(self$axesDefinition$B)
    cc <- rlang::parse_expr(self$axesDefinition$C)

    # filter data
    if(!is.null(self$dataFilter)){
      df <- rlang::parse_expr(self$dataFilter)
      WRD <- dplyr::filter(WRD,!!df)
    }

    # parse A, B and C
    WRD %>% dplyr::mutate(A = !!aa, B = !!bb, C = !!cc) %>%
      {.} -> WRD

    # Append the plotting coordinates
    res <- ternaryCoordinates(WRD$A, WRD$B, WRD$C,
                      rotation = self$ternaryRotation, scale = self$ternaryScale)

    # Split back
    lbl <- data.frame(WRD[,lbl_nm])
    row.names(lbl) <- WRD$ID_x

    # Results
    plottingCoords<- cbind(res,WRD[,wr_nm])
    row.names(plottingCoords) <- WRD$ID_x
  }

  return(list(plottingCoords=plottingCoords,
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

  if (requireNamespace("GCDkit", quietly = TRUE)) {
    #If GCDkit is available, we can use nice versions...
    if(!is.null(self$axesName[[W]])){
      wlab <- GCDkit::annotate(self$axesName[[W]])
    }else{
      wlab <- GCDkit::annotate(self$axesDefinition[[W]])
    }
  }else{
    #Without GCDkit, use plain labels
    if(!is.null(self$axesName[[W]])){
      wlab <- self$axesName[[W]]
    }else{
      wlab <- self$axesDefinition[[W]]
    }
  }

  return(wlab)

}
