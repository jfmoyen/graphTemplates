# Each element of the template is an object, with subclasses appropriate to chat they are

######## Class definition ###########

new_templateElement <- function(te){
  #' Constructor for templateElement class
  #' @rdname templateElement

  structure(te,class="templateElement")
}


validate_templateElement <- function(te){
  #' Validator for templateElement class
  #' @rdname templateElement
  # cat("No checks performed, I trust you...")
  invisible(te)
}


templateElement <- function(te){
#' a class that stores template elements
#'
#' @description
#' A (graphical) element of a template
#'
#' Each \link{graphTemplate} is made of various information, the core of
#' which is the "template" section. This contains individual elements (lines, texts, etc.)
#' that are all defined as a templateElement object.
#' templateElements have subclasses such as lines, text... defining how they will be treated.
#'
#' Figaro does have functionalities to interpret and plot diverse elements, so no
#' further work is required (expect passing the template to Figaro, see ?plotFigaro).
#' In ggplot however, there is an extra step of converting the templateElement to
#' ggplot graphical primitives (ggproto).
#'
#' @param te An input list to convert to a template element
#' @export
  te<-new_templateElement(te)

  te <- validate_templateElement(te)

  invisible(te)
}


# print.templateElement <- function(x,...){
#   #' @export
#   cat("a",x$plotFun,"\n")
# }

#### Generic
showTemplateElement <- function(self,options) {
  #' Controls the display of individual template elements
  #'
  #' This method can be used to remove template elements based on switches.
  #' If ALL the switches are TRUE, or is the element has no switch, the element is kept,
  #'  Otherwise, if ANY of the switches is FALSE,  the element is removed from
  #'  the template - i.e. the method returns NULL
  #'
  #' @param self a template element
  #' @param options a named boolean vector containing the values of the switches.
  #'
  #' @export
  UseMethod("showTemplateElement")
}

#### Implementation
showTemplateElement.templateElement <- function(self,options){
  #' @rdname showTemplateElement
  #' @export

  # If the element has a switch field, do something...
  if(any(names(self) == "switch")){

    if( all(options[self$switch]) ){
      # All the switches are on.
      # Remove the tag, keep the element
      self$switch <- NULL
    }else{
      # else drop the element
      self <- NULL
    }
  }
  return(self)
}

#### Generic
styleTemplateElement <- function(self,options) {
  #' Controls the styling of individual template elements
  #'
  #' This method can be used to change various graphical parameters
  #' controlling the aspect of template graphical elements. For the moment,
  #' only color can be changed.
  #'
  #' @param self a template element
  #' @param options a named vector containing the values of the optional colours, ltys or lwds.
  #' It should contain values for "colDefault", "ltyDefault" and "lwdDefault"
  #' (although there are safeguards)
  #'
  #' @returns A templateElement, with col, lty and lwd values added
  #' or modified according to specs
  #'
  #' @details Styling of col, lwd and lty works similarly. In all cases, the logic is the following:
  #' \itemize{
  #' \item If there is no field of that name, it will be added and given the default value,
  #' supplied in options (or defaulting to "black", "solid" and 1).
  #' \item If there is a hard-coded value (e.g. a well-conformed color name, a string such
  #' as "dashed", etc.), it will be kept.
  #' \item If there is a variable defined value (e.g. "plotco1"), it will be replaced,
  #' in order of preference:
  #' \itemize{
  #' \item By the user-supplied calue for it, in the form options=c(pltcol="blue", ... )
  #' \item By the default defined in the json template
  #' \item By the default, that should come in options as options = c(colDefault="white");
  #' or by the "default default" (black, solid and 1).
  #' }
  #' }
  #' Any element that has col, lty or lwd will be processed.
  #'
  #' @export

  UseMethod("styleTemplateElement")
}

styleTemplateElement.templateElement <- function(self,options){
  #' @rdname styleTemplateElement
  #' @export

  ######### COLOURS ############
  # If no default have been supplied, add them
  if(!("colDefault" %in% names(options))){
    options["colDefault"] <- "black"
  }

  if(any(names(self) == "col")){
    # This element has a color definition, let's dig further
    if(!isColor(self$col)){
      # If the color is a legitimate colour name, don't touch it.
      # However, if it is not...
      if(any(names(options) == self$col)){
        # The user has supplied an equivalence
        self$col <- options[self$col]
      }else{
        # Default
        self$col <- options["colDefault"]
      }
    }
  }else{
      # Default
      self$col <- options["colDefault"]}

  ######### LINETYPES ############
  # If no default have been supplied, add them
  if(!("ltyDefault" %in% names(options))){
    options["ltyDefault"] <- "solid"
  }

  if(any(names(self) == "lty")){
    # This element has a lty definition, let's dig further
    if(!(self$lty %in% c("blank", "solid", "dashed",
                         "dotted", "dotdash", "longdash",
                         "twodash", seq(0:9)) ) ){
      # If the lty is a legitimate name, don't touch it.
      # However, if it is not...
      if(any(names(options) == self$lty)){
        # The user has supplied an equivalence
        self$lty <- options[self$lty]
      }else{
        # Default
        self$lty <- options["ltyDefault"]
      }
    }
  }else{
    # Default
    self$lty <- options["ltyDefault"]}

  ######### LINEWIDTH ############
  # If no default have been supplied, add them
  if(!("lwdDefault" %in% names(options))){
    options["ltyDefault"] <- 1
  }

  if(any(names(self) == "lwd")){
    # This element has a lwd definition, let's dig further
    if(!(is.numeric(self$lwd) ) ){
      # If the lwd is a number, don't touch it.
      # However, if it is not...
      if(any(names(options) == self$lwd)){
        # The user has supplied an equivalence
        self$lwd <- options[self$lwd]
      }else{
        # Default
        self$lwd <- options["lwdDefault"]
      }
    }
  }else{
    # Default
    self$lwd <- options["lwdDefault"]}

  return(self)
}

# styleTemplateElement.arrows <-
# styleTemplateElement.abline <-
# styleTemplateElement.lines <- function(self,options){
#   #' @export
#
#   self <- NextMethod()
#
#
#
#   return(self)
# }

styleTemplateElement.text <- function(self,options){
  #' Style a text template element
  #'
  #' @inherit styleTemplateElement
  #'
  #' @section Text: In addition, if a text element is given as an expression it will
  #' be evaluated (allowing to use formatted text in templates)
    #'
  #' @export

  self <- NextMethod()

  # Evaluate text elements given as expressions
  self$text <-sapply(self$text,
         function(z){
            if(grepl("expression",z) ){
                z<-eval(parse(text = z) )
                }
            return(z)},
    simplify=T
  )

  return(self)
}


