.onLoad <- function(libname, pkgname){

  cat("Reading template helpers..\n")

  # Source all R code found under /templates

  rootdir <- system.file("templates",package="graphTemplates")
  toc <- list.files(rootdir,recursive=T)
  idx <- endsWith(toc,".R")|endsWith(toc,".r")

  Rfiles <- toc[idx]

  lapply(Rfiles,function(z){
    cat("sourcing",z,"..")
    source(paste0(rootdir,"/",z))
    cat(".ok\n")
  })

  # source()
  cat("..done\n")

  # Size conversions
  pointsize = par("ps") # the default R pointsize
  .pt <- 2.845276 # 1 point

  # in ggplot the sizes are given in mm, with the conversion factor in .pt
  # So the adjustment factor for text is
  options("text_size_magic_nbr" = pointsize/.pt)

  # ggplot has default symbol size of 5 mm (see GeomPoint$default_aes ? ) compared to <pointsize> pts in basic R
  # So to emulate the graphs we must account for that
  options("point_size_magic_nbr" = pointsize/5)

  options("lwd_size_magic_nbr" = 0.1) # Looks ok

}
