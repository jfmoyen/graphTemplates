##NB There is deliberately no import from ggplot2 to avoid creating a dependency !


#### Generic

makeggTemplate <- function(self,...){
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

  UseMethod("makeggTemplate")
}

makeggTemplate.graphTemplate<- function(self,...){
  #' @export
  #' @rdname makeggTemplate

  cat("No plotting methods for ",class(self)[1],"implemented yet. Quitting." )

  return()
}


makeggTemplate.binary<- function(self,...){
  #' @export
  #' @rdname makeggTemplate

  # Axis preparation
  ## X and Y scale (log or natural)

  xlog <- self$log=="x"||self$log=="xy"
  ylog <- self$log=="y"||self$log=="xy"

  # Default
  scale_x <- scale_x_continuous(expand=c(0,0))
  scale_y <- scale_y_continuous(expand=c(0,0))

  if(xlog){
    scale_x <- scale_x_log10(expand=c(0,0))
  }

  if(ylog){
    scale_y <- scale_y_log10(expand=c(0,0))
  }

  tpl <- list(
    map(self$template,gglayerTemplateElement),
    scale_x,
    scale_y,
    xlab(makeName(self,"X")),
    ylab(makeName(self,"Y")),
    coord_cartesian(xlim=self$limits$X,ylim=self$limits$Y),
    ggtitle(self$fullName)
  )

  return(tpl)
  return()
}

makeggTemplate.ternary<- function(self,...){
  #' @export
  #' @rdname makeggTemplate

  # Axis preparation
  ## X and Y scale (log or natural)

  xlog <- self$log=="x"||self$log=="xy"
  ylog <- self$log=="y"||self$log=="xy"

  # Default
  scale_x <- scale_x_continuous(expand=c(0,0))
  scale_y <- scale_y_continuous(expand=c(0,0))

  if(xlog){
    scale_x <- scale_x_log10(expand=c(0,0))
  }

  if(ylog){
    scale_y <- scale_y_log10(expand=c(0,0))
  }

  tpl <- list(
    map(self$template,gglayerTemplateElement),
    scale_x,
    scale_y,
    xlab(makeName(self,"X")),
    ylab(makeName(self,"Y")),
    coord_fixed(xlim=self$limits$X,ylim=self$limits$Y,clip="off"),
    ggtitle(self$fullName)
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
  #' want more granularity, it is better to use \link{makeggTemplate} and do the rest
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

  # Data transformation
  wrdata <- self$dataTransform(ee$wrdata)

  # Correct colours
  ee$lbl[,"Colour"] <- sapply(ee$lbl[,"Colour"],
         function(z){
           if(is.numeric(z)){palette()[z]}else{z}
           })

  # Merge all in a tibble
  WRD <- GCDkitToTibble(wrdata,ee$lbl)

  # String to expression, for ggplot data-masking
  xx <- rlang::parse_expr(self$axesDefinition$X)
  yy <- rlang::parse_expr(self$axesDefinition$Y)

  # filter data
  if(!is.null(self$dataFilter)){
    df <- rlang::parse_expr(self$dataFilter)
    WRD <- dplyr::filter(WRD,!!df)
  }

  # Get template
  tpl <- makeggTemplate(self)

  # Build the plot
  plt <- ggplot2::ggplot(WRD,aes(x=!!xx,y=!!yy))+
    geom_point(aes(colour=Colour,
                   fill=Colour,
                   shape=Symbol,
                   size=Size*getOption("point_size_magic_nbr")))+
    scale_shape_identity()+
    scale_color_identity()+
    scale_fill_identity()+
    scale_linetype_identity()+
    scale_size_identity()+
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

  # Data transformation
  wrdata <- self$dataTransform(ee$wrdata)

  # Correct colours
  ee$lbl[,"Colour"] <- sapply(ee$lbl[,"Colour"],
                              function(z){
                                if(is.numeric(z)){palette()[z]}else{z}
                              })

  # Merge all in a tibble
  WRD <- GCDkitToTibble(wrdata,ee$lbl)

  # Ternary coordinates
  # String to expression, for ggplot data-masking
    aa <- rlang::parse_expr(self$axesDefinition$A)
    bb <- rlang::parse_expr(self$axesDefinition$B)
    cc <- rlang::parse_expr(self$axesDefinition$C)

    # A, B and C if data-transformed
    WRD %>% mutate(A = !!aa, B = !!bb, C = !!cc) %>%
      {.} -> WRD

    # Append the plotting coordinates
    WRD %>% bind_cols(ternaryCoordinates(WRD$A, WRD$B, WRD$C,
                                     rotation = self$ternaryRotation, scale = self$ternaryScale)
    ) %>%
      {.} -> WRD

    # filter data
    if(!is.null(self$dataFilter)){
      df <- rlang::parse_expr(self$dataFilter)
      WRD <- dplyr::filter(WRD,!!df)
    }

    # Get template
    tpl <- makeggTemplate(self)

    # Build the plot
    plt <- ggplot2::ggplot(WRD,aes(x.data,y=y.data))+
      geom_point(aes(colour=Colour,
                     fill=Colour,
                     shape=Symbol,
                     size=Size*getOption("point_size_magic_nbr")))+
      scale_shape_identity()+
      scale_color_identity()+
      scale_fill_identity()+
      scale_linetype_identity()+
      scale_size_identity()+
      tpl+
      theme_gcdkit()+
      theme(axis.line = element_blank(), ##Remove the real axes
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust = 0, vjust = 6))

     print(plt)

  invisible(plt)
}
