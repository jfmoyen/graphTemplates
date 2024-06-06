
### SETUP ###

library(GCDkit)
library(graphTemplates)

data("atacazo")
accessVar(atacazo)

#############
# low-level functions: simple usage

tt <- parseJsonTemplate("Cabanis",template_options=c(showText=F))
tt <- addTernaryAxes(tt)
plotFigaro(tt,WR,labels)


ttr <- parseJsonTemplate("AFM.json")
ttr <- addTernaryAxes(ttr)
plotFigaro(ttr,WR,labels)

tt <- parseJsonTemplate("PearceNbThYb") # Note that the .json suffic can be omited...
plotFigaro(tt,WR,labels)

ttr <- parseJsonTemplate("Mullen")
ttr <- addTernaryAxes(ttr)
plotFigaro(ttr,WR,labels)

###########
# low-level functions: options

## Template options

tt <- parseJsonTemplate("test2",template_options=c(plotthis=T))
plotFigaro(tt,WR,labels)

tt <- parseJsonTemplate("test2",template_options=c(plotthis=T,plotthat=F))
plotFigaro(tt,WR,labels)

## Color options

# LarochePlut has a style default defined for pltcol2
tt <- parseJsonTemplate("LarochePlut.json")
plotFigaro(tt,WR,labels)

tt <- parseJsonTemplate("LarochePlut.json",style_options=c(pltcol2="purple"))
plotFigaro(tt,WR,labels)

tt <- parseJsonTemplate("test2",
                              template_options=c(plotthis=T,plotthat=T),
                              style_options = c(pltcol1="red",pltcol3="blue"))
plotFigaro(tt,WR,labels)

## Line type options
tt <- parseJsonTemplate("Batchelor")
plotFigaro(tt,WR,labels)

tt <- parseJsonTemplate("Batchelor",style_options=c(linedash="solid"))
plotFigaro(tt,WR,labels)

tt <- parseJsonTemplate("Cabanis",template_options=c(showText=F),
                              style_options=c(arrowwidth=3))
tt <- addTernaryAxes(tt)
plotFigaro(tt,WR,labels)

## Transformation options
tt<-parseJsonTemplate("optionsDemo")
plotFigaro(tt,WR,labels)

tt<-parseJsonTemplate("optionsDemo",transform_options=c(doubleBB=T))
plotFigaro(tt,WR,labels)

## Niceties with ternary diagrams
tt <- parseJsonTemplate("AFM")
tt <- addTernaryOrnaments(tt)
plotFigaro(tt,WR,labels)

tt <- parseJsonTemplate("AFM")
tt <- addTernaryOrnaments(tt,grid=T)
plotFigaro(tt,WR,labels)

# Changing my mind...
tt <- addTernaryOrnaments(tt,axes=F,apicesNames=F,grid=T,grinterval=5,grcol="red",grlty="solid")
plotFigaro(tt,WR,labels)

# Ticks
tt <- parseJsonTemplate("AFM")
tt <- addTernaryOrnaments(tt,ticks=T)
plotFigaro(tt,WR,labels)

## Rotating triangles
tt <- parseJsonTemplate("Cabanis")
tt <- addTernaryOrnaments(tt)
tt <- rotateTernaryTemplate(tt,rotation=30)
plotFigaro(tt,WR,labels)
# It's not guaranteed to be nice...

### High level function using GCDkit emulation
# plotDiagram.json is the GCDkit connector, it is not a package function,
# must me loaded manually...
plotDiagram.json("DebonBA")
plotDiagram.json("AFM")

############
# Adding points
WRata <- WR
lblata <- labels
data(blatna)
accessVar("blatna")

plotDiagram.json("DebonBA")
addData(tt,WRata,"black",3)

# Adding lines
plotDiagram.json("DebonPQ")
addLine(c(0,100),c(-100,100))

######## Test all diagrams
templ_dir <- system.file("json_templates",package="graphTemplates")
templ_list <- list.files(templ_dir,recursive=T,include.dirs = F)
sapply(templ_list,
       function(thediag){
         cat("loading",thediag,"...")
         tt <- parseJsonTemplate(thediag)
         cat("plotting\n")
         plotFigaro(tt,WR,labels)
         cat(thediag,"OK\n")
         } )

### Test ternary diagrams only
sapply(templ_list,
       function(thediag){
         cat("loading",thediag,"...")
         tt <- parseJsonTemplate(thediag)
         if(class(tt)=="ternary"){
           cat("plotting\n")
           tt <- addTernaryOrnaments(tt,grid=T)
           tt <- rotateTernaryTemplate(tt,rotation=30)
           plotFigaro(tt,WR,labels)
           cat(thediag,"OK\n")
         }else{
           cat("skipping\n")
         }

       } )


### TESTING ZONE
tt<-parseJsonTemplate("Frost_fig1")
plotFigaro(tt,WR,labels)

tt<-parseJsonTemplate("Frost_fig1",transform_options=c(FeOonly=T))
plotFigaro(tt,WR,labels)

tt<-parseJsonTemplate("projBiot",template_options=c(idealMins=T))
tt <- addTernaryOrnaments(tt)
plotFigaro(tt,WR,labels)

##NB plotDiagram("projbioplot",idealmins=T,new=T)
