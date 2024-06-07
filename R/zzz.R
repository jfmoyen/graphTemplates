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
}
