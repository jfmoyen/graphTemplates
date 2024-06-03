### Call from GCDkit


plotDiagram_json <- function(diagram,select.samples=TRUE,new=TRUE,main=NULL,width=6.5,height=6.5,pointsize=10,
                             bg="transparent",autoscale=TRUE,interactive=FALSE,...){
  #' A replacement for GCDkit's plotDiagram, with the same API
  #'
  #' In principle, it should take the same arguments and return the same Figaro
  #' graph, so it aims to be a transparent replacement.

  # This is a pure GCDkit function ! It should fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Honour GCDkit showText settings
}



#' @param template_options Named vector, options (switches) to change
#' @param transform_options Additional parameters to pass to the data transformation function
#' @param style_options
