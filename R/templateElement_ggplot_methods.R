##NB There is deliberately no import from ggplot2 to avoid creating a dependency !


#### Generic
gglayerTemplateElement <- function(self) {
  #' Turn a templateElement into a ggplot layer
  #'
  #' This method converts a templateElement into a ggplot player, using
  #' ggplot2::annotate (i.e. they are annotation layers)
  #' It assumes that all defaults (col, lwd and lty at least)
  #' are set properly.
  #'
  #' @param self a template element
  #'
  #' @export
  UseMethod("gglayerTemplateElement")
}

#### Implementation
gglayerTemplateElement.templateElement <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export

  if(!is.null(class(self)[1])){
    cat("WARNING: graphical elements of type",class(self)[1],"are not implemented yet. Dropping.\n")
    }

  return(NULL)
}

gglayerTemplateElement.lines <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export
  #'

  gg_el <- ggplot2::annotate(geom="path",x=self$x, y=self$y,
                             colour=self$col,linetype=self$lty,
                             size=self$lwd*getOption("lwd_size_magic_nbr") )
  return(gg_el)
}

gglayerTemplateElement.polygon <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export
  #'
  gg_el <- ggplot2::annotate(geom="polygon",
                             x=self$x,y=self$y,fill=self$col,color=NA)
  return(gg_el)
}

gglayerTemplateElement.arrows <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export
  #'


  # Convert the numeric code of base::arrows() to the ends arg of grid::arrow()
  ends <- switch(self$code,
                 "first",
                 "last",
                 "both")

  gg_el <- ggplot2::annotate(geom="segment",
                             x=self$x0,y=self$y0,
                             xend=self$x1,yend=self$y1,
                             color=self$col,linetype=self$lty,
                             size=self$lwd*getOption("lwd_size_magic_nbr"),
                             arrow=ggplot2::arrow(angle=self$angle,length=ggplot2::unit(self$length,"inches"),ends=ends))
  return(gg_el)
}

gglayerTemplateElement.text <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export
  #'

  # There is no reliable default for these, so if they are not supplied we must define them...
  if(is.null(self$srt)){angle<-0}else{angle<-self$srt}
  if(is.null(self$cex)){cex<-1}else{cex<-self$cex}

  # adj is a bit more complex, as ggplot does not take a vector but wants 2 scalars
  # also pos can override adj in grapics::text
  vadj <- 0.5
  if(is.null(self$adj)){
    hadj <- 0.5
  }else{
    hadj<-self$adj[1]
    if(length(self$adj==2)){
      vadj <- self$adj[2]
    }
  }

  # Override adj if pos is called (help(graphics::text))
  if(!is.null(self$pos)){
    if(self$pos==1){hadj=0.5;vadj=1} # below
    if(self$pos==2){hadj=0;vadj=0.5} # left
    if(self$pos==3){hadj=0.5;vadj=0} # above
    if(self$pos==4){hadj=1;vadj=0.5} # right
  }

  # Convert the graphics::text font definitions to ggplot compatible
  if(is.null(self$font)){the_font<-"plain"}else{
    the_font<- switch(self$font,
                      "plain",
                      "bold",
                      "italic",
                      "bold.italic")
  }


  gg_el <- ggplot2::annotate("text",
                             label=self$text,
                             x=self$x,y=self$y,
                             colour=self$col,
                             size=cex*getOption("text_size_magic_nbr"),
                             angle=angle,
                             hjust = hadj,
                             vjust = vadj,
                             fontface = the_font)



  return(gg_el)
}

gglayerTemplateElement.abline <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export
  #'

  # abline, hline, vline are not supported by annotate so we can not use annotation here!
  # https://github.com/tidyverse/ggplot2/issues/4719

  if(!is.null(self$v)){
      gg_el <- ggplot2::annotate("segment",
                          x = self$v, xend = self$v, y = -Inf, yend = Inf,
                          linetype=self$lty,
                          size=self$lwd*getOption("lwd_size_magic_nbr"),colour=self$col)
                  }

  if(!is.null(self$h)){
    gg_el <- ggplot2::annotate("segment",
                               y = self$h, yend = self$h, x = -Inf, xend = Inf,
                               linetype=self$lty,
                               size=self$lwd*getOption("lwd_size_magic_nbr"),colour=self$col)
      }
# Oblique ablines...
  if(!is.null(self$a)){
    cat("Oblique ablines not supported for now\n")
    # gg_el <- geom_abline(ggplot2::aes(intercept = self$a,slope=self$b),
    #                      linetype=self$lty,size=self$lwd*getOption("lwd_size_magic_nbr"),colour=self$col)
    gg_el <- NULL
    }

  return(gg_el)
}

gglayerTemplateElement.points <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export
  #'

  if(is.null(self$cex)){cex<-1}else{cex<-self$cex}

  gg_el <- ggplot2::annotate(geom="point",
                             self$x,self$y,
                             colour=self$col,
                             shape=self$pch,
                             size=cex*getOption("point_size_magic_nbr"))

  return(gg_el)
}

gglayerTemplateElement.curve <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export
  #'

  # As far as I know, not found in any diagram...

  gg_el <- ggplot2::annotate(geom="function",fun = as.expression(self$equation),
                             colour=self$col,linetype=self$lty,
                             size=self$lwd*getOption("lwd_size_magic_nbr"))

  return(gg_el)
}

gglayerTemplateElement.reservoirs <- function(self){
  #' @rdname gglayerTemplateElement
  #' @export
  #'

  # To be developed
  # Complex interactions with GCDkit...
  if(!is.null(class(self)[1])){
    cat("WARNING: graphical elements of type",class(self)[1],"are not implemented yet. Dropping.")
  }

  return(NULL)
}


