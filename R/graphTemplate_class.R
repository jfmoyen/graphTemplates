######## Class definition ###########

new_graphTemplate <- function(name=NULL,fullName=NULL,meta=NULL,Rdialect=NULL,
                              dataTransform=NULL,hook=NULL,dataFilter=NULL,
                              axesDefinition=NULL,axesName=NULL,log=log,limits=NULL,
                              template=NULL){
  #' Class constructor for graphTemplate
  #' @rdname graphTemplate

  gt <- list(name=name,
             fullName=fullName,
             meta=meta,
             Rdialect=Rdialect,
             dataTransform=dataTransform,
             hook=hook,
             dataFilter=dataFilter,
             axesDefinition = axesDefinition,
             axesName = axesName,
             log=log,
             limits = limits,
             template=template)

  structure(gt,class=c("unknown","graphTemplate"))
}


validate_graphTemplate <- function(self){
  #' Internal validation function for graphTemplate
  #' @rdname graphTemplate
  # cat("No checks performed, I trust you...\n")
  invisible(self)
}


graphTemplate <- function(name=NULL,fullName=NULL,meta=NULL,Rdialect=NULL,
                          dataTransform=function(z){return(z)},
                          hook=function(self,wrdata,lbl){return(list(self=self,wrdata=wrdata,lbl=lbl))},
                          dataFilter=NULL,
                          axesDefinition=NULL,axesName=NULL,log=NULL,limits=NULL,template=NULL,
                          diagramType="unknown"){
  #' A class that stores all the information allowing to reproduce a graph
  #'
  #' @details
  #' A graphTemplate is a list, with the following elements:
  #' \itemize{
  #' \item a name (short, typically the name of the json file it is loaded from)
  #' \item a fullName, more explicit
  #' \item meta, itself a list containing various metadata such as a reference, template creator, etc.
  #' \item Rdialect, when the diagram requires functions (for data transformation) belonging to
  #' a certain package (gcdkit, probably). The plotter is expected to complain if the right library
  #' is not loaded...
  #' \item dataTransform a function that takes a data.frame and returns the plotting coordinates (it is
  #' usually as simple as function(x){return(x)}, but more elaborate functions can be used. The
  #' json parser is able to interpret, to a degree, what is found in its dataTransform field and combine it
  #' with user-supplied arguments).
  #' \item dataFilter. Some graphs plot only data obeying certain conditions, such as
  #' SiO2 < 54 %. This should be supplied as a string (fit for filtering by the plotter).
  #' \item axesDefinition the axes, expressed in terms of the (col)names of the data
  #' \item axesName as they will be displayed on the graph. Can be prettier... GCDkit will
  #' attempt (using GCDkit::annotate) to prettify them. ggplot is not as clever (but it is
  #' easy to modify an axis name).
  #' \item log. "", "x", "y" or "xy" in usual R convention.
  #' \item limits, a list with X and Y components, two vectors of two elements...
  #' \item template the template itself, a collection of elements of type templateElement
  #' }
  #' In addition, a graphTemplate has a subclass, defined by diagramType: binary or ternary.
  #' This will be interpreted by the plotter.
  #' In fact, since a graphTemplate is a list, it can contain any other element. However,
  #' the creator function takes only these arguments, and only them will be used for plotting...
  #' Additionally, a graphTemplate can be of subclass binary or ternary.
  #' @param All the elements of the list, as given above, correspond to parameters of the
  #' class constructor.
  #' @export

  gt<-new_graphTemplate(name,fullName,meta,Rdialect,
                        dataTransform,hook,dataFilter,axesDefinition,axesName,log,limits,
                        template)

  class(gt)<- c(diagramType,"graphTemplate")

  validate_graphTemplate(gt)

  return(gt)
}

############## Print methods ##################
print.graphTemplate <- function(self,...){
  #' @export
  #' @rdname graphTemplate

  nb <- length(self$template)
  cat(nb,"graphical elements"," ($template)\n")
  print(table(unlist(lapply(1:nb,function(i){class(self$template[[i]])[1]}))))
  cat("\n")

  optionsUsed<-unique(unlist(lapply(self$template,function(te){
    items <- unlist(lapply(te,function(z){names(z)}))
    c(items["col"],items["lty"],items["lwd"])
  })))
  optionsUsed <- optionsUsed[!is.na(optionsUsed)]
  optionsUsed <- setdiff(optionsUsed,c("colDefault","ltyDefault","lwdDefault"))

  if(length(optionsUsed)>0){
    cat("Graphical options used:\n")
    cat(optionsUsed)
    cat("\nYou may want to explore the template to see what they do...\n\n")
  }

  if(any(names(self$template) == "clssf")){
    cat(length(self$template$clssf$use),"fields used for classification\n\n")
  }
  cat("Remember that you can use \'unclass()\' to see the actual object\n")
}

print.unknown <- function(self,..){
  #' @export
  #' @rdname graphTemplate
  cat("A diagram template of unknwon type called ")
  cat(self$name,"\n")
  cat(self$fullName,"\n")
  cat("(see $meta for more details)\n")
  cat("-----------------------------------\n")
  NextMethod()
}

print.binary <- function(self,..){
  #' @export
  #' @rdname graphTemplate
   cat("A binary diagram template called ")
  cat(self$name,"\n")
  cat(self$fullName,"\n")
  cat("(see $meta for more details)\n")
  cat("-----------------------------------\n")
  if(!is.null(self$dataFilter)){
    cat("Plots only samples where",self$dataFilter,"\n")
  }
  cat("Axes:\n")
  cat("X:",self$axesDefinition$X,"from",self$limits$X[1],"to",self$limits$X[2])
  if(!is.null(self$axesName$X)){
    cat(" (displayed as ",self$axesName$X,")")
  }
  if(self$log=="x"||self$log=="xy"){
    cat(" (in log scale)")
  }
  cat("\n")
  cat("Y:",self$axesDefinition$Y,"from",self$limits$Y[1],"to",self$limits$Y[2])
  if(!is.null(self$axesName$Y)){
    cat(" (displayed as ",self$axesName$Y,")")
  }
  if(self$log=="y"||self$log=="xy"){
    cat(" (in log scale)")
  }
  cat("\n")
  cat("-----------------------------------\n")
  NextMethod()
}

print.ternary <- function(self,..){
  #' @export
  #' @rdname graphTemplate
  cat("A ternary diagram template called ")
  cat(self$name,"\n")
  cat(self$fullName,"\n")
  cat("(see $meta for more details)\n")
  cat("-----------------------------------\n")
  if(!is.null(self$dataFilter)){
    cat("Plots only samples where",self$dataFilter,"\n")
  }
  cat("Axes:\n")
  cat("A (bottom left):",self$axesDefinition$A)
  if(!is.null(self$axesNames$A)){
    cat(" (also known as ",self$axesNames$A,")")
  }
  cat("\n")
  cat("B (top):",self$axesDefinition$B)
  if(!is.null(self$axesNames$B)){
    cat(" (also known as ",self$axesNames$B,")")
  }
  cat("\n")
  cat("C (bottom right):",self$axesDefinition$C)
  if(!is.null(self$axesNames$C)){
    cat(" (also known as ",self$axesNames$C,")")
  }
  cat("\n")
  cat("-----------------------------------\n")
  NextMethod()
}

print.plate <- function(self,..){
  #' @export
  #' @rdname graphTemplate
  cat("A plate template called ")
  cat(self$name,"\n")
  cat(self$fullName,"\n")
  cat("(see $meta for more details)\n")
  cat("-----------------------------------\n")
  cat(self$nbslots,"sub-plots,",self$nrow,"row(s) and",self$ncol,"col(s)\n")
  cat("Sub-plots:\n")
  for(i in 1:self$nbslots){
    cat("Slot",i,":",self$plateSlots[[i]]$fullName,"\n")
  }
  cat("use <template>$plateSlots[[1]] (etc) to see individual plots\n")
}

#### Generic
#' addTernary
#'
#' Add ternary information (axes) to a template
#'
#' @param self a template
#' @param scale scaling factor
#' @param rotation Angle to rotate
#' @export
#' @rdname addTernaryAxes.graphTemplate
addTernaryAxes <- function(self,scale,rotation) {
  UseMethod("addTernaryAxes")
}

addTernaryAxes.graphTemplate <- function(self,scale,rotation){
  #' Add axes to a ternary template
  #'
  #' @details
    #' This method is used to add ternary (pseudo) axes to a graphTemplate. The plotter
    #' should remove the true axes (and ensure a square aspect ratio). The function
    #' eventually will be able to rotate a ternary graph by an arbitrary amount.
    #'
  #' @param self a graphTemplate
  #' @param scale The scale of the triangle: normally from 0 to 1 but in some contexts
  #' the traingle could be defined as 0-100 (%). Probably deprecated.
  #' @param rotation in degrees, angle to rotate the graph (in progress).
  #' @export

  msg <- paste("Can only add ternary axes on a ternary template!\n")
  stop(msg)
}

addTernaryAxes.ternary <- function(self,scale=1,rotation=0){
  #' @export
  #' @rdname addTernaryAxes.graphTemplate
  pseudoAxes=list(type="lines",x=c(0,1,0.5,0),y=c(0,0,sqrt(3)/2,0),col="black")

  # Include in template
  self$template <- c(self$template,pseudoAxes=list(pseudoAxes))

  return(self)

}

#TODO Ticks scaling rotation
# rotate template ?

