show_transform<-function(diag){
  graphDef<-json_loader(diag)
  print(graphDef$dataTransform)
}

templ_dir <- system.file("json_templates",package="jsonGraphTemplates")
templ_list <- list.files(templ_dir,recursive=T,include.dirs = F)

sapply(templ_list,
       function(thediag){show_transform(thediag)
                          } )


##################
## All templates

templ_dir <- system.file("templates",package="graphTemplates")
templ_list <- list.files(templ_dir,recursive=T,include.dirs = F)
idx <- endsWith(templ_list,".json")
templ_list <- templ_list[idx]

# Who is who...
sapply(templ_list,
       function(thediag){
         tt <- parseJsonTemplate(thediag)
         cat(thediag,"\n------------------------------\n")
         cat(class(tt)[1],"\n")
         nb <- length(tt$template)
         print(table(unlist(lapply(1:nb,function(i){class(tt$template[[i]])[1]}))))
         cat("\n")
         cat("log:",tt$log,"\n")
       } )
