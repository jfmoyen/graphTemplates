##NB There is deliberately no import from ggplot2 to avoid creating a dependency !


#### Generic

graph_template <- function(self,...){
  #' Build an empty graph template
  #'
  #' @details
  #' This function assembles the "static" elements of a template (lines, text)
  #' as well as axis and scale definitions and convert them for GCDkit. It does
  #' not map aesthetics nor calculate data points, the user must do it in this case
  #' (see examples). Alternately, use \link{plotgg} for a more global, but less
  #' flexible solution.
  #' @param self a graphTemplate
  #' @param ... probably not used
  #'
  #' @export

  UseMethod("graph_template")
}

graph_template.graphTemplate<- function(self,...){
  #' @export
  #' @rdname graph_template

  cat("No plotting methods for ",class(self)[1],"implemented yet. Quitting." )

  return()
}


graph_template.binary<- function(self,...){
  #' @export
  #' @rdname graph_template

  # Axis preparation
  ## X and Y scale (log or natural)

  xlog <- self$log=="x"||self$log=="xy"
  ylog <- self$log=="y"||self$log=="xy"

  # Default
  scale_x <- ggplot2::scale_x_continuous(expand=c(0,0))
  scale_y <- ggplot2::scale_y_continuous(expand=c(0,0))

  if(xlog){
    scale_x <- ggplot2::scale_x_log10(expand=c(0,0))
  }

  if(ylog){
    scale_y <- ggplot2::scale_y_log10(expand=c(0,0))
  }

  tpl <- list(
    purrr::map(self$template,gglayerTemplateElement),
    scale_x,
    scale_y,
    ggplot2::xlab(makeName(self,"X")),
    ggplot2::ylab(makeName(self,"Y")),
    ggplot2::coord_cartesian(xlim=self$limits$X,ylim=self$limits$Y),
    ggplot2::ggtitle(self$fullName)
  )

  return(tpl)
  return()
}

graph_template.ternary<- function(self,...){
  #' @export
  #' @rdname graph_template

  tpl <- list(
    purrr::map(self$template,gglayerTemplateElement),
    ggplot2::scale_x_continuous(expand=c(0,0)),
    ggplot2::scale_y_continuous(expand=c(0,0)),
    ggplot2::xlab(makeName(self,"X")),
    ggplot2::ylab(makeName(self,"Y")),
    ggplot2::coord_fixed(xlim=self$limits$X,ylim=self$limits$Y,clip="off"),
    ggplot2::ggtitle(self$fullName)
  )

  return(tpl)
  return()
}


#### Generic

plotgg <- function(self,wrdata,lbl,new,...){
  #' Plot a template in ggplot2 context
  #'
  #' @details
  #' The function that does the real job... this function converts the template
  #' into ggplot objects and add points on it, obeying GCDkit-style conventions
  #' (Symbol, Colour etc). If you are not coming from a GCDkit context and/or
  #' want more granularity, it is better to use \link{graph_template} and do the rest
  #' by hand.
  #'
  #' Symbols, colours, and size are taken from variables Symbol, Colour and Size in
  #' lbl (itself potentially coming from GCDkit, or from the global variable labels).
  #'
  #' @param self a graphTemplate
  #' @param wrdata,lbl The usual GCDkit WR and labels objects, default to what is
  #' in the global environment.
  #' @param new should control whether a new window is opened, but ignored for now.
  #' @param ... probably not used
  #'
  #' @export
  UseMethod("plotgg")
}


plotgg.graphTemplate<- function(self,wrdata,lbl,new,...){
 #' @export
 #' @rdname plotgg

  cat("No plotting methods for ",class(self)[1],"implemented yet. Quitting." )

  return()
}

plotgg.binary<- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),new,...) {
  #' @export
  #' @rdname plotgg
  #' @importFrom grDevices palette
  #'

  # This is a pure ggplot2 function ! It should fail if ggplot2 is not here
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Execute hook function
   ee <- self$hook(self,wrdata=wrdata,lbl=lbl)

  self <- ee$self

  # Correct colours
  ee$lbl[,"Colour"] <- sapply(ee$lbl[,"Colour"],
         function(z){
           if(is.numeric(z)){grDevices::palette()[z]}else{z}
           })

  # Point coordinates
  pc <- pointCoordinates(self,ee$wrdata,ee$lbl,mode="ggplot")
  plottingDS <- dplyr::bind_cols(pc$plottingCoords,pc$lbl)

  # Get template
  tpl <- graph_template(self)

  # Build the plot
  plt <- ggplot2::ggplot(plottingDS,ggplot2::aes(x.data,y=y.data))+
    ggplot2::geom_point(ggplot2::aes(colour=Colour,
                   fill=Colour,
                   shape=Symbol,
                   size=Size*getOption("point_size_magic_nbr")))+
    ggplot2::scale_shape_identity()+
    ggplot2::scale_color_identity()+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_linetype_identity()+
    ggplot2::scale_size_identity()+
    tpl+
    theme_gcdkit()

  print(plt)

  invisible(plt)


   # Suppress the "real" axes
   # TODO, if self$suppresAxes - but this would need a proper implementation of
   # box, axis etc.
   #   plt<-plt+theme(axis.line = element_blank(),
   #                  panel.border = element_blank(),
   #                  axis.ticks = element_blank(),
   #                  axis.title = element_blank(),
   #                  axis.text.x = element_blank(),
   #                  axis.text.y = element_blank(),
   #                  aspect.ratio = 1)
}


plotgg.ternary<- function(self,wrdata=WR,lbl=get("labels",.GlobalEnv),new,...) {
  #' @export
  #' @rdname plotgg
  #'

  # This is a pure ggplot2 function ! It should fail if ggplot2 is not here
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

    # Execute hook function
  ee <- self$hook(self,wrdata=wrdata,lbl=lbl)

  self <- ee$self

  # Correct colours
  ee$lbl[,"Colour"] <- sapply(ee$lbl[,"Colour"],
                              function(z){
                                if(is.numeric(z)){palette()[z]}else{z}
                              })

  # Point coordinates
  pc <- pointCoordinates(self,ee$wrdata,ee$lbl,mode="ggplot")
  plottingDS <- dplyr::bind_cols(pc$plottingCoords,pc$lbl)

    # Get template
    tpl <- graph_template(self)

    # Build the plot
    plt <- ggplot2::ggplot(plottingDS,ggplot2::aes(x.data,y=y.data))+
      ggplot2::geom_point(ggplot2::aes(colour=Colour,
                     fill=Colour,
                     shape=Symbol,
                     size=Size*getOption("point_size_magic_nbr")))+
      ggplot2::scale_shape_identity()+
      ggplot2::scale_color_identity()+
      ggplot2::scale_fill_identity()+
      ggplot2::scale_linetype_identity()+
      ggplot2::scale_size_identity()+
      tpl+
      theme_gcdkit()+
      ggplot2::theme(axis.line = ggplot2::element_blank(), ##Remove the real axes
            panel.border = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(hjust = 0, vjust = 6))

     print(plt)

  invisible(plt)
}
