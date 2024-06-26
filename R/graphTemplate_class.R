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
  #' \item hook a function ran during plotting, that allows to change some template elements.
  #' See example in Frost_fig1.json
  #' \item dataFilter. Some graphs plot only data obeying certain conditions, such as
  #' SiO2 < 54 %. This should be supplied as a string (fit for filtering by the plotter).
  #' \item axesDefinition the axes, expressed in terms of the (col)names of the data
  #' \item axesName as they will be displayed on the graph. Can be prettier... GCDkit will
  #' attempt (using GCDkit::annotate) to prettify them. ggplot is not as clever (but it is
  #' easy to modify an axis name).
  #' \item log. "", "x", "y" or "xy" in usual R convention.
  #' \item suppressAxes, a boolean allowing to suppress axes drawing
  #' \item limits, a list with X and Y components, two vectors of two elements...
  #' \item template the template itself, a collection of elements of type templateElement
  #' }
  #' In addition, a graphTemplate has a subclass, defined by diagramType: binary or ternary.
  #' This will be interpreted by the plotter.
  #'
  #' A ternary template also includes ternaryScale (deprecated?) and ternaryRotation (degrees),
  #' indicating by how much the template will be scaled and/or rotated before plotting.
  #'
  #' A plate template is simpler - it contains metadata and options, and then nbslots, nrow, ncol, and
  #' a list plateSlots containing individual graph templates.
  #'
  #' In fact, since a graphTemplate is a list, it can contain any other element. However,
  #' the creator function takes only these arguments, and only them will be used for plotting...
  #' Additionally, a graphTemplate can be of subclass binary or ternary.
  #' @param name,fullName,meta,Rdialect,dataTransform,hook,dataFilter,axesDefinition,axesName,log,limits,template,diagramType the elements of the list, as given in details,
  #'  correspond to parameters of the class constructor.
  #'
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
addTernaryOrnaments <- function(self,...) {
  #' Add axes to a ternary template
  #'
  #' @details
  #' This method is used to add various elements to a ternary template, in particular
  #' ternary (pseudo) axes, names of the apices, ticks and grid. The plotting function
  #' should remove the true axes (and ensure a square aspect ratio).
  #' Elements will honour the template rotation.
  #'
  #' @param self a graphTemplate
  #' @param axes,ticks,grid,apicesNames : should we add the relevant elements?
  #' @param interval.gr,lwd.gr,lty.gr,col.gr spacing between grid lines, and their graphical properties
  #' @param length.ti,interval.ti,lwd.ti,lty.ti,col.ti spacing between ticks, length, and their graphical properties
  #' @param lwd.ax,lty.ax,col.ax graphical properties of the pseudoaxes
  #' @param padding Extra space to add around the plot, to allow depicting the apices labels (left, top, right, bottom)
  #' @param ... to ternary method
  #'
  #' @returns updated version of self
  #'
  #' @export

  UseMethod("addTernaryOrnaments")
}

addTernaryOrnaments.graphTemplate <- function(self,...){
  #' @export
  #' @rdname addTernaryOrnaments

  msg <- paste("Can only add ternary axes on a ternary template!\n")
  stop(msg)
}

addTernaryOrnaments.ternary <- function(self,
                                        axes = T,
                                        lwd.ax=1,lty.ax="solid",col.ax="black",
                                        apicesNames = T,
                                        ticks = F,
                                        length.ti=0.03,interval.ti=10,lwd.ti=1,lty.ti="solid",col.ti="black",
                                        grid = F,
                                        interval.gr=10,lwd.gr=0.5,lty.gr="dotted",col.gr="grey",
                                        padding=c(0.03,0.03,0.03,0.05) ){
  #' @export
  #' @rdname addTernaryOrnaments

  # Ticks
  if(ticks){
    grd <- ternaryTicks(length=length.ti,interval=interval.ti,lwd=lwd.ti,lty=lty.ti,col=col.ti)

    grd<-lapply(grd,
                function(z){
                  rotateTemplateElement(z,rotation=self$ternaryRotation,
                                        scale=self$ternaryScale)
                })

    self$template <- c(grd,self$template)
  }

  # Grid
  if(grid){
    grd <- ternaryGrid(interval=interval.gr,lwd=lwd.gr,lty=lty.gr,col=col.gr)

    grd<-lapply(grd,
                function(z){
                  rotateTemplateElement(z,rotation=self$ternaryRotation,
                                        scale=self$ternaryScale)
                })

    self$template <- c(grd,self$template)
  }

  # The (pseudo)axes
  if(axes){

    pseudoAxes<-templateElement(
            list(plotFun="lines",
                    x=c(0,1,0.5,0),
                    y=c(0,0,sqrt(3)/2,0),
                    lty=lty.ax,lwd=lwd.ax,col=col.ax))

      pseudoAxes<-rotateTemplateElement(pseudoAxes,rotation=self$ternaryRotation,
                               scale=self$ternaryScale)

      self$template <- c(pseudoAxes=list(pseudoAxes),self$template)
  }

  if(apicesNames){
      # Custom axes names
      alab <- makeName(self,"A")
      blab <- makeName(self,"B")
      clab <- makeName(self,"C")

      A<-templateElement(list(plotFun="text",
                              x=0,y=-padding[4],text=alab,adj=c(0.5,0),col=col.ax))

      B<-templateElement(list(plotFun="text",
                               x=0.5,y=sqrt(3)/2+padding[2],text=blab,adj=c(0.5,0.5),col=col.ax))
      C<-templateElement(list(plotFun="text",
                              x=1,y=-padding[4],text=clab,adj=c(0.5,0),col=col.ax))

      A<-rotateTemplateElement(A,rotation=self$ternaryRotation,
                            scale=self$ternaryScale)
      B<-rotateTemplateElement(B,rotation=self$ternaryRotation,
                            scale=self$ternaryScale)
      C<-rotateTemplateElement(C,rotation=self$ternaryRotation,
                            scale=self$ternaryScale)

      # Include in template
      self$template <- c(self$template,A=list(A),B=list(B),C=list(C))
  }

  return(self)
}


#### Generic

rotateTernaryTemplate <- function(self,rotation,scale,padding,setup=F) {
  #' @export
  #' @rdname rotateTernaryTemplate.graphTemplate
  #'
  UseMethod("rotateTernaryTemplate")
}

rotateTernaryTemplate.graphTemplate <- function(self,rotation,scale,padding,setup=F){
  #' Add axes and other graphical elements to a ternary template
  #'
  #' @details
  #' This method allows to rotate a template by a certain angle (in degrees). It will
  #' rotate the template elements itself; if using pointCoordinates method, the
  #' points will also end up in the right place. This works in plotFigaro
  #' and plotgg.
  #' If the user wants to do things manually (in ggplot), the rotation for data has to be implemented manually;
  #' points should be then plotted with something like
  #' transformedData <- tpl$dataTransform(WR)
  #' plottingCoords <- ternaryCoordinates(transformedData$a.data,
  #'                               transformedData$b.data,
  #'                                transformedData$c.data,
  #'                             tpl$ternaryRotation,tpl$ternaryScale)
  #'
  #' ..geom_points(data=plottingCoords,aes(x.data,y.data))
  #'
  #' @param self a graphTemplate
  #' @param scale The scale of the triangle: normally from 0 to 1 but in some contexts
  #' the triangle could be defined as 0-100 (%). Probably deprecated.
  #' @param rotation in degrees, angle to rotate the graph.
  #' @param padding Extra space to add on the sides, c(left,top,right,bottom)
  #' @param setup If T, use during setup. Just rotate. If F, use when rotating an
  #' existing plot. This will update self$ternaryRotation.
  #' @export

  cat("Can only rotate ternary template! Nothing happened\n")
  return(self)
}

rotateTernaryTemplate.ternary <- function(self,rotation=0,scale=1,padding=c(0.03,0.03,0.03,0.05),setup=F){
  #' @export
  #' @rdname rotateTernaryTemplate.graphTemplate

  if(!setup){
    self$ternaryRotation <- self$ternaryRotation + rotation
    self$scale <- scale
  }

  # Update bounding box
  self$limits <- ternaryBoundingBox(rotation=rotation,scale=scale,padding=padding)

  # Rotate the graphical elements
  tpl<-lapply(self$template,
         function(z){
            rotateTemplateElement(z,rotation=rotation,scale=scale)
         })

  self$template <- tpl

  return(self)

}

