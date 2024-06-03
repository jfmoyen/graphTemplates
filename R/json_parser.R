####### json reader #################


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

#### JSON PARSER ####
parse_template_fromjson<-function(json,path=NULL,
                                  meta=c("details","reference","url","templateAuthor","templateConversion","templateHistory"),
                                  template_options=NULL,
                                  transform_options=NULL,
                                  style_options=c("colDefault"="black",
                                                  ltyDefault="solid",
                                                  lwdDefault=1),
                                  doFilter=T){
  #' Parse a template loaded from json, converting the options as required
  #'
  #' This function combines a "raw" list, loaded from json file, with user options.
  #' It performs the required modifications, such as changing colours or
  #' removing elements, and returns a properly formatted graphTemplate object.
  #'
  #' @param json Name of the template file (if the .json extension is missing, it will be added automatically)
  #' @param path Path to json file. By default the package files.
  #' @param meta the names of the fields from the json file that should be kept as meta
  #' @param template_options Named vector. They contain the switches to activate, or
  #' desactivate, controlling the display of some graph elements (see ?jsonDiagramFormat)
  #' @param transform_options Additional parameters to pass to the data transformation function
  #' @param style_options Options to control the styling of some elements of the template.
  #' (see ?jsonDiagramFormat).
  #' @param doFilter if the template contains a filter (eg SiO2 > 45), should we respect it?
  #'
  #' @details
    #' Most of the heavy loading is done at etemplate element level. A template is
    #' primarily a collection (list) of templateElement, stored in self$template.
    #' Each will be further processed, based on its class (lines, text...)
    #'
  #' @returns a graphTemplate object.
  #'
  #' @export

  # Get the template
  tpl <- load_json_file(json,path)

  # Prepare the switching options
  switching_options <- unlist(tpl$optionDefaults)
  switching_options[names(template_options)]<-template_options[names(template_options)]

  # Prepare the styling options
  # Default is what is found in the template, overidden by user
  # Note that we include de fact a default value!
  st_opt <- unlist(tpl$styleDefaults)
  st_opt[names(style_options)] <- style_options[names(style_options)]

  # Prettify each template element
  if(is.null(tpl$template)){
    template_nice <- NULL
  }else{
    template_nice<-lapply(tpl$template,
                          function(z){
                            class(z) <- c(z$plotFun, "templateElement",class(z))
                            zz<-styleTemplateElement(z,st_opt)
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

  # A better definition of log
  if(is.null(tpl$log)){lg<-""}else{lg<-tpl$log}
  # The return object
   ee<- graphTemplate(
      name = tpl$name,
      fullName = tpl$fullName,
      meta = tpl[meta],
      Rdialect = tpl$RDialect,
      axesDefinition = tpl$axesDefinition,
      axesName = tpl$axesName,
      log=lg,
      limits = tpl$limits,
      template = template_nice,
      diagramType = tpl$diagramType,
      dataFilter = if(doFilter){tpl$dataFilter}
    )

 # Add the data transform function (if there is one)
   if(!is.null(tpl$dataTransform)){
     #Build the function
     trfFunction<-try(get(tpl$dataTransform),silent=T)
     if(class(trfFunction)=="try-error"){
       stop(cat("Function",tpl$dataTransform,"is required but has not been found\n"))
       }
     trfParams <- tpl$dataTransformParams

     # So we have a function that returns a function that executes a function
     # (holy Molly...)
     fun <- function(trfParams,transform_options){
       function(wrdata) {
         args <- c(list(wrdata),trfParams,transform_options)
         do.call(trfFunction,args=args )
       }
     }

     # Include it in the object (at least we execute one...)
     ee$dataTransform <- fun(trfParams,transform_options)
 }

 return(ee)
}

