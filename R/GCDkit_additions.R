########## Functions for data transformation ################
# These require GCDkit

#### Combined Debon calculations
DebonCalcFull<-function(wrdata){
  #' This function combines calculation of millications and of Debon parameters
  #' @export
  #' @param wrdata A matrix with WR data

# This is a pure GCDkit function ! It should fail if GCDkit is not here
if (!requireNamespace("GCDkit", quietly = TRUE)) {
  stop(
    "Package \"GCDkit\" must be installed to use this function.",
    call. = FALSE
  )
}

  return(GCDkit::DebonCalc(GCDkit::millications(wrdata)) )
}

#### Frost data transform

FrostFeNbr<-function(wrdata,FeOonly=F){
  #' Calculate Fe nbr for Frost plot
  #' @export
  #' @param wrdata A matrix with whole-rock composition, probably coming from GCDkit
  #' @param FeOonly if true, use FeO strictly (see details)
  #' @returns a matrix with column FeNbr
  #' @details
  #' Frost's Fe nbr is said to use FeO. It is however unclear whether this
  #' should be FeO strictly, or FeOt (all iron as FeO).
  #'

  swt <- all(!is.na(wrdata[,"FeO"])&!is.na(wrdata[,"Fe2O3"]) ) & FeOonly

  if(swt){
    FeNbr <- GCDkit::calcCore("FeO/(FeO+MgO)"  ,where="wrdata",redo=F)$results
  }else{
    FeNbr <- GCDkit::calcCore("FeOt/(FeOt+MgO)",where="wrdata",redo=F)$results
      }

  return(cbind(wrdata,FeNbr))
}


FrostASI<-function(wrdata,originalDef=F,zeroP=T){
  #' Calculate ASI based on Frost's definition
  #' @export
  #' @param wrdata A matrix with whole-rock composition, probably coming from GCDkit
  #' @param originalDef Use Frost's original (incorrect) equation? (see details)
  #' @param zeroP Boolean. If true, replace missing P2O5 values by na.
  #' @returns a matrix with column ASI.
  #' @details
  #' There is a well-known typo in Frost's paper, where the equation for
  #' ASI is given with an incorrect coefficient (not respecting toechiometry).
  #' This is generally acknowledged to be incorrect, but it is possible to
  #' reinstall the original behaviour if wished.
  #'

  # This is a pure GCDkit function ! It should fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this function.",
      call. = FALSE
    )
  }

    milli <- millications(wrdata,print=F)

    p<-milli[,"P2O5"]
    if(zeroP){
      p[is.na(p)]<-0
    }

  if(originalDef){
    ASI<-milli[,"Al2O3"]/(milli[,"CaO"]-1.67*p+milli[,"Na2O"]+milli[,"K2O"])
  }else{
    ASI<-milli[,"Al2O3"]/(2*milli[,"CaO"]-3.33*p+milli[,"Na2O"]+milli[,"K2O"])
  }

  return(cbind(wrdata,ASI))
}
