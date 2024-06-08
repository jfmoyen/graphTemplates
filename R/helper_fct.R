#######################################################
#
#          Helper functions
#
#######################################################

#### is color ? ####
#' Check whether a string (or strings) is a valid color name
#' https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
#'
#' @importFrom grDevices col2rgb
#' @param x character vector, the strings to test
#' @returns T or F
isColor <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
             error = function(e) FALSE)
  })
}


#### ggplot theme to emulate GCDkit ####
theme_gcdkit <- function() {
  #' A ggplot theme that resembles GCDkit's
  #' @details nice and clean
  #' @export


  ggplot2::theme_bw(base_size=12, base_family="Avenir") %+replace%
    ggplot2::theme(
      panel.background  = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
      legend.key = ggplot2::element_rect(fill="transparent", colour=NA),
      panel.grid = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size=14, face="bold", colour = "black"),
      axis.title.y = ggplot2::element_text(size=14, face="bold", colour = "black",angle=90),
      axis.text.x = ggplot2::element_text(size=12, colour = "black"),
      axis.text.y = ggplot2::element_text(size=12, colour = "black",angle=90),
      axis.ticks.length = ggplot2::unit(.25, "cm")
    )
}

GCDkitToTibble <- function(gcdWR=WR,gcdlabels=get("labels",.GlobalEnv)){
  #' Convert GCDkit-style data (WR and labels) into a tibble
  #'
  #' @param gcdWR WR matrix from GCDkit
  #' @param gcdlabels labels data.frame from GCDkit
  #'
  #' @details this is a tidyverse function. It will fail if tidyverse is not loaded.
  #' There is no explicit import in the function (to avoid creating a dependency during installation),
  #' so errors may be cryptic, and probably related to magrittR pipe ("%>%")
  #'
  #' @returns a tibble with all the data
  #' @export

  sampleNames <- rownames(gcdWR)
  DS <- tibble::as_tibble(gcdWR) %>%
    dplyr::mutate(ID=sampleNames, .before=1 ) %>%
    dplyr::bind_cols(gcdlabels)

  return(DS)
}

TibbleToGCDkit <- function(WRD,nameCol=names(WRD)[1]){
  #' (attempts to) convert a tibble into two GCDkit tables.
  #'
  #' @details
    #' This function does its best, putting numerical data in WR and the
    #' rest in labels, as per GCDkit convention
    #'
  #' @param WRD a tibble with all the info
  #' @param nameCol containing the sample names (ID), the first one by default
  #'
  #' @returns a list with two components, WR and labels
  #' @export

  # Columns (not) containing numbers
  txt_col <- names(WRD[sapply(WRD,class) != "numeric"])
  num_col <- names(WRD[sapply(WRD,class) == "numeric"])

  # GCDkit special variables, even if they are numbers !
  spc_col <- intersect(colnames(WRD), c(nameCol,"Symbol","Colour","Size","Alpha"))

  # Columns going to labels
  lbl_col <- unique(c(txt_col,spc_col))
  lbl_col <- setdiff(lbl_col,nameCol)


  # Columns going to WR
  WR_col <- setdiff(num_col,spc_col)

  WR <- as.matrix(WRD[,WR_col,drop=F])
  rownames(WR) <- WRD[,nameCol,drop=T]

  lbl <- data.frame(WRD[,lbl_col,drop=F])
  rownames(lbl) <- WRD[,nameCol,drop=T]

  return(list(WR=WR,labels=lbl))
  }

