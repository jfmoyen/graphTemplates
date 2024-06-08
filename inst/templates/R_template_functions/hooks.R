### Ad-hoc hooks to execute during plotting and alter graph templates

my_test_hook <- function(self,wrdata,lbl){
  #' Just testing!
  #'
  #' @param self The template
  #' @param wrdata WR data
  #' @param lbl labels
  #' @export

  cat("I am a test hook!\n")

  return(list(self=self,wrdata=wrdata,lbl=lbl))
}


FrostFeHook <- function(self,wrdata,lbl,FeOonly=F){
  #' Hook function to change the position of the line in Frost Figure 1
  #'
  #' The position of the line is adjusted based on the Fe component
  #' @param self The template
  #' @param wrdata WR data
  #' @param lbl labels
  #' @param FeOonly if true, use FeO strictly (see details)
  #' @details
  #' Frost's Fe nbr is said to use FeO. It is however unclear whether this
  #' should be FeO strictly, or FeOt (all iron as FeO).
  #' @returns modified template, wrdata, lbl
  #' @export

  swt <- all(!is.na(wrdata[,"FeO"])&!is.na(wrdata[,"Fe2O3"]) ) & FeOonly

  if(swt){
    self$axesName$Y<-"FeO/(FeO+MgO)"
    self$template$lines1$y <- self$template$lines1$y - 0.04
  }else{
    self$axesName$Y<-"FeOt/(FeOt+MgO)"
      }

  return(list(self=self,wrdata=wrdata,lbl=lbl))
}

projBiotHook <- function(self,wrdata,lbl){
  #' Hook function to tweak Moyen et al. proj from biotite
  #'
  #' @param self The template
  #' @param wrdata WR data
  #' @param lbl labels
  #'
  #' @returns modified template, wrdata, lbl
  #' @export

  self$limits$X <- c(-2.03,1.4)
  self$limits$Y <- c(-1.03,1.03)

  if(!is.null(self$template$B)){
    self$template$B$adj <- c(0,0)
  }

  if(!is.null(self$template$C)){
      self$template$C$adj <- c(0.5,1)
  }

  return(list(self=self,wrdata=wrdata,lbl=lbl))

  }
