
### SETUP ###

library(GCDkit)

data("atacazo")
accessVar(atacazo)

setwd("D:/GitProjects/graphTemplates")

document()
load_all()

#############
# low-level functions: simple usage

tt <- parse_template_fromjson("Cabanis",template_options=c(showText=F))
tt <- addTernaryAxes(tt)
plotFigaro(tt,WR,labels)


ttr <- parse_template_fromjson("AFM.json")
ttr <- addTernaryAxes(ttr)
plotFigaro(ttr,WR,labels)

tt <- parse_template_fromjson("PearceNbThYb") # Note that the .json suffic can be omited...
plotFigaro(tt,WR,labels)

ttr <- parse_template_fromjson("Mullen")
ttr <- addTernaryAxes(ttr)
plotFigaro(ttr,WR,labels)

###########
# low-level functions: options

## Template options
tt <- parse_template_fromjson("AFM")
plotFigaro(tt,WR,labels)

tt <- parse_template_fromjson("test2",template_options=c(plotthis=T))
plotFigaro(tt,WR,labels)

tt <- parse_template_fromjson("test2",template_options=c(plotthis=T,plotthat=F))
plotFigaro(tt,WR,labels)

## Color options

# LarochePlut has a style default defined for pltcol2
tt <- parse_template_fromjson("LarochePlut.json")
plotFigaro(tt,WR,labels)

tt <- parse_template_fromjson("LarochePlut.json",style_options=c(pltcol2="purple"))
plotFigaro(tt,WR,labels)

tt <- parse_template_fromjson("test2",
                              template_options=c(plotthis=T,plotthat=T),
                              style_options = c(pltcol1="red",pltcol3="blue"))
plotFigaro(tt,WR,labels)

## Line type options
tt <- parse_template_fromjson("Batchelor")
plotFigaro(tt,WR,labels)

tt <- parse_template_fromjson("Batchelor",style_options=c(linedash="solid"))
plotFigaro(tt,WR,labels)

tt <- parse_template_fromjson("Cabanis",template_options=c(showText=F),
                              style_options=c(arrowwidth=3))
tt <- addTernaryAxes(tt)
plotFigaro(tt,WR,labels)

############
# Adding points
WRata <- WR
lblata <- labels
data(blatna)
accessVar("blatna")

plotDiagram_json("DebonPQ")
addData(tt,WRata,"black",3)

# Adding lines
plotDiagram_json("DebonPQ")
addLine(c(0,100),c(-100,100))

######## Test all diagrams
templ_dir <- system.file("json_templates",package="graphTemplates")
templ_list <- list.files(templ_dir,recursive=T,include.dirs = F)
sapply(templ_list,
       function(thediag){
         cat("loading",thediag,"...")
         tt <- parse_template_fromjson(thediag)
         cat("plotting\n")
         plotFigaro(tt,WR,labels)
         cat(thediag,"OK\n")
         } )
