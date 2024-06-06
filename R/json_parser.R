#############################################
#
## The json parser and its subcomponents ####
#
#############################################


#### Top-level json parser ####
###############################

parseJsonTemplate <-function(json,path=NULL,
                             plateMeta=c("details","reference","url","templateAuthor","templateConversion","templateHistory"),
                             ...){
  #' Parse a template loaded from json, converting the options as required
  #'
  #' This function combines a "raw" list, loaded from json file, with user options.
  #' It performs the required modifications, such as changing colours or
  #' removing elements, and returns a properly formatted graphTemplate object.
  #'
  #' @param json Name of the template file (if the .json extension is missing, it will be added automatically)
  #' @param path Path to json file. By default the package files.
  #' @param plateMeta the names of the fields from the json file that should be kept
  #'  as metadata for the plate (if we are looking at a plate). Single plot-level metadata
  #'  should be defined using meta, and are passed to make_single_template via ...
  #' @param ... further options to be processed, see \link{make_single_template}
  #' @details
  #' This is a thin wrapper on make_single_template. It main job is to decide whether
  #' we are looking at a single plot, or a plate, and send the template or templates
  #' to make_single_template
  #'
  #' @returns a graphTemplate object.
  #'
  #' @importFrom grDevices n2mfrow
  #' @export

  # Get the template
  tpl_raw <- load_json_file(json,path)

  if(tpl_raw$diagramType == "plate"){
    tpl <- make_plate_template(tpl_raw,path=path,plateMeta=plateMeta,...)
  }else{
    # Any other type is only one diagram
    tpl <- make_single_template(tpl_raw,...)
  }

  return(tpl)
}


#### Internal functions ####
############################

#### Load json template ####

load_json_file<-function(json,path=NULL){
  #' Reads a json template file into a list
  #'
  #' @description A graphTemplate can be defined as a json file. This function
  #' load a json (from package's directory or user-supplied location),
  #' performs some basic syntaxic checks and returns a list for further processing.
  #'
  #' @param json Name of the template file (if the .json extension is missing, it will be added automatically)
  #' @param path Path to json file
  #' @details
  #' If no path is supplied, the function will look in the json_template
  #' directory of the package folder, and its subdirectories. It will return
  #' the first match, starting by the top-level folder and exploring
  #' sub-folders alphabetically.
  #'
  #' @importFrom jsonlite read_json
  #' @export


  ## If no path is defined, look for the template in template dir & sub-dir
  if(is.null(path)){
    toc <- list.dirs(system.file("json_templates",package="graphTemplates"))
    for(i in (1:length(toc) )){
      ii <- paste(toc[i],json,sep="/")
      if(file.exists(ii)){ thejson <- ii; break() }
      ii <- paste(toc[i],paste0(json,".json"),sep="/")
      if(file.exists(ii)){ thejson <- ii; break() }
    }

  }else{
    thejson<-paste(path,json,sep="/")
  }

  graphDef<-jsonlite::read_json(thejson,simplifyVector = T)

  ### Check the validity of the json

  # All switches must have defaults
  if( !is.null(graphDef$optionDefaults)){
    if( !all.equal(sort(names(graphDef$optionDefaults)),
                   sort(names(graphDef$optionSwitches))
                   ) ){
      stop("Error in json template:\n
           all switches MUST have defaults")
    }
  }

  # Other syntaxic checks ?
  # ...

  return(graphDef)
}

#### Convert the input list into a graphTemplate ####
make_single_template <- function(tpl_raw,
                                 meta=c("details","reference","url","templateAuthor","templateConversion","templateHistory"),
                                 template_options=NULL,
                                 transform_options=NULL,
                                 style_options=c("colDefault"="black",
                                                 ltyDefault="solid",
                                                 lwdDefault=1),
                                 doFilter=T){
  #' Convert a raw list into a proper graphTemplate
  #'
  #' @param tpl_raw a list, that will be converted to a graphTemplate.
  #' Probably loaded from json, using \link{load_json_file}
  #' @param meta the names of the fields from the input list that should be kept as meta
  #' @param template_options Named vector. They contain the switches to activate, or desactivate,
  #' controlling the display of some graph elements (see \link{jsonDiagramFormat}).
  #' @param transform_options Additional parameters to pass to the data transformation function
  #' @param style_options Options to control the styling of some elements of the template (see \link{jsonDiagramFormat}).
  #' @param doFilter if the template contains a filter (eg SiO2 > 45), should we respect it?
  #'
  #' @export

  # Prepare the switching options
  switching_options <- unlist(tpl_raw$optionDefaults)
  switching_options[names(template_options)]<-template_options[names(template_options)]

  # Prepare the styling options
  # Default is what is found in the template, overidden by user
  # Note that we include de fact a default value!
  st_opt <- unlist(tpl_raw$styleDefaults)
  st_opt[names(style_options)] <- style_options[names(style_options)]

  # Prettify the template
  template_nice <- process_template_options(tpl_raw$template,switching_options,st_opt)

  # Assemble the return object
  gt <- graphTemplate(
    Rdialect = tpl_raw$RDialect,
    axesDefinition = tpl_raw$axesDefinition,
    axesName = tpl_raw$axesName,
    limits = tpl_raw$limits,
    template = template_nice,
    diagramType = tpl_raw$diagramType,
    dataFilter = if(doFilter){tpl_raw$dataFilter}
  )

  ## Add the missing bits
  # Suppress axes
  if(is.null(tpl_raw$suppressAxes)|| !(tpl_raw$suppressAxes)){
    gt$suppressAxes <- FALSE}
  else{
    gt$suppressAxes <- TRUE
  }

  # Metadata etc
  meta <- intersect(meta,names(tpl_raw))

  # Include in template
  gt$meta <- tpl_raw[meta]

  # Name
  if(is.null(tpl_raw$name)){gt$name<-""}else{gt$name<-tpl_raw$name}
  if(is.null(tpl_raw$fullName)){gt$fullName<-""}else{gt$fullName<-tpl_raw$fullName}

  # A better definition of log
  if(is.null(tpl_raw$log)){gt$log<-""}else{gt$log<-tpl_raw$log}

  # Construct the data transform function
  if(!is.null(tpl_raw$dataTransform)){
    gt$dataTransform <- make_function(tpl_raw$dataTransform,c(tpl_raw$dataTransformParams, transform_options ))
  }

  # Add the hook function
  if(!is.null(tpl_raw$hook)){
    gt$hook <- make_function(tpl_raw$hook,c(tpl_raw$dataTransformParams, transform_options ),fullArgs=T)
  }

  return(gt)
}

#### Convert the input list into a graphTemplate ####
make_plate_template <- function(tpl_raw,path,
                                 plateMeta=c("details","reference","url","templateAuthor","templateConversion","templateHistory"),
                                 template_options=NULL,
                                 transform_options=NULL,
                                 style_options=c("colDefault"="black",
                                                 ltyDefault="solid",
                                                 lwdDefault=1),
                                 doFilter=T,...){
  #' Convert a raw list into a proper graphTemplate
  #'
  #' @param tpl_raw a list, that will be converted to a graphTemplate.
  #' Probably loaded from json, using \link{load_json_file}
  #' @param plateMeta the names of the fields from the json file that should be kept
  #'  as metadata for the plate
  #' @param template_options Named vector. They contain the switches to activate, or desactivate,
  #' controlling the display of some graph elements (see \link{jsonDiagramFormat}).
  #' @param transform_options Additional parameters to pass to the data transformation function
  #' @param style_options Options to control the styling of some elements of the template (see \link{jsonDiagramFormat}).
  #' @param doFilter if the template contains a filter (eg SiO2 > 45), should we respect it?
  #' @param ... Further arguments passed to make_single_template(),
  #' i.e. arguments applying to individual templates.
  #'
  #' @export

  ## Combine the plate-level options with the user options
  # Switching options
  switching_options <- unlist(tpl_raw$optionDefaults)
  switching_options[names(template_options)]<-template_options[names(template_options)]

  # Styling options
  st_opt <- unlist(tpl_raw$styleDefaults)
  st_opt[names(style_options)] <- style_options[names(style_options)]

  # Several diagrams, that need to be treated in turn
  nbslots <- length(tpl_raw$plateSlots)
  tpl <- list(plateSlots=NULL)
  tpl$plateSlots<-lapply(1:nbslots, function(i) {
    json_subplot <- tpl_raw$plateSlots[[i]]$diagram
    component <- load_json_file(json_subplot,path)

    ee <- make_single_template(component,
                           template_options=switching_options,
                           style_options=st_opt,...)

    # Rescale if needed, i.e. if limits are supplied in the template
    if( !is.null(tpl_raw$plateSlots[[i]]$limits) ){
      ee$limits <- tpl_raw$plateSlots[[i]]$limits
    }
    return(ee)
  })
  class(tpl)<- c("plate","graphTemplate")

  # Metadata etc
  plateMeta <- intersect(plateMeta,names(tpl_raw))

  # Include in template
  tpl$meta <- tpl_raw[plateMeta]

  # Name
  if(is.null(tpl_raw$name)){tpl$name<-""}else{tpl$name<-tpl_raw$name}
  if(is.null(tpl_raw$fullName)){tpl$fullName<-""}else{tpl$fullName<-tpl_raw$fullName}

  # Geometry
  tpl$nbslots <- nbslots

  tpl$ncol <- tpl_raw$ncol
  tpl$nrow <- tpl_raw$nrow

  if(is.null(tpl$nrow)||is.null(tpl$ncol)){
    tpl$ncol <- grDevices::n2mfrow(nbslots)[1]
    tpl$nrow <- grDevices::n2mfrow(nbslots)[2]
  }

  return(tpl)
}


#### Modify a template based on options ####
process_template_options<-function(tpl_objects,
                                   switching_options=NULL,
                                   style_options=c("colDefault"="black",
                                                   ltyDefault="solid",
                                                   lwdDefault=1)){
  #' Internal function, modifies a template based on user options
  #'
  #' @param tpl_objects A list containing elements that can be interpreted as templateElement
  #' @param switching_options Named vector. They contain the switches to activate, or desactivate,
  #' controlling the display of some graph elements (see \link{jsonDiagramFormat}).
  #' @param style_options Options to control the styling of some elements of the template (see \link{jsonDiagramFormat}).
  #' @details each element of the input list will be converted into a templateElement object.
  #' If it is switched "off", it will be removed. If appropriate, its properties
  #' (col, lty and lwd) will be modified.
  #'
  #' @returns a list of templateElement, modified and "filtered" as required
  #' @export

  # Prettify each template element
  if(is.null(tpl_objects)){
    template_nice <- NULL
  }else{
    template_nice<-lapply(tpl_objects,
                          function(z){
                            class(z)<-c("templateElement",class(z))
                            zz<-styleTemplateElement(z,style_options)
                            zzz<-showTemplateElement(zz,switching_options)
                            return(zzz)
                          })
  }
  # Remove empty elements
  template_nice <- Filter(Negate(is.null),template_nice)

  # In the case of an empty template, return one dummy element to make Figaro happy
  if(length(template_nice)==0){
    template_nice <- list(nothing=list(plotFun=""))
  }

  return(template_nice)
}

#### Add a function in the template ####

make_function <- function(fnName,params=NULL,fullArgs=F){
  #' Internal function to build a function
  #'
  #' @param fnName In string form, the name of function
  #' @param params Parameters to pass to the fnName function
  #' @param fullArgs If true, the function generated will take self (a template),
  #' wrdata and lbl as arguments. Otherwose it will take only wrdata.
  #' @returns a function
  #'
  #' @export

  # Early exit if the argument is NULL
  if(is.null(fnName)){return(NULL)}

  #Build the function
  trfFunction<-try(get(fnName),silent=T)
  if(class(trfFunction)=="try-error"){
    stop(cat("Function",fnName,"is required but has not been found\n"))
  }

  # So we have a function that returns a function that executes a function
  # (holy Molly...)
  if(fullArgs){
    fun <- function(params){
      function(self,wrdata,lbl) {
        args <- c(list(self,wrdata,lbl),params)
        do.call(trfFunction,args=args )
      }
    }
  }else{
    fun <- function(params){
      function(wrdata) {
        args <- c(list(wrdata),params)
        do.call(trfFunction,args=args )
      }
    }
  }


  # Include it in the object (at least we execute one...)
  return( fun(params) )

}

