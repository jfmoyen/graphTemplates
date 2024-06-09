
### PLAIN GCDKIT ###

library(GCDkit)
library(graphTemplates)

data("atacazo")
accessVar(atacazo)

#############
# low-level functions: simple usage

tt <- parseJsonTemplate("Cabanis",template_options=c(showText=F))
tt <- addTernaryOrnaments(tt)
plotFigaro(tt,WR,labels)


ttr <- parseJsonTemplate("AFM.json")
ttr <- addTernaryOrnaments(ttr)
plotFigaro(ttr,WR,labels)

tt <- parseJsonTemplate("PearceNbThYb") # Note that the .json suffic can be omited...
plotFigaro(tt,WR,labels)

ttr <- parseJsonTemplate("Mullen")
ttr <- addTernaryOrnaments(ttr)
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
tt <- addTernaryOrnaments(tt)
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
tt <- addTernaryOrnaments(tt,axes=F,apicesNames=F,grid=T,interval.gr=5,col.gr="red",lty.gr="solid")
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
connectorFile <- paste0(system.file("extra",package="graphTemplates"),"/GCDkit_connector.R")
source(connectorFile)

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

### GGPLOT ###

library(tidyverse) # We use also some function from dplyr, magrittr etc.
library(GCDkit) # It is wise to load GCDkit last, as it masks some functions
# from other packages, such as annotate - and it is not always well behaved
# and does not explicitely call GCDkit::annotate, so bad things may happen
# if GCDkit is masked...
library(graphTemplates)

data("atacazo")
accessVar(atacazo)

### High level interface emulating GCDkit behaviour

tt<-parseJsonTemplate("Frost_fig1")
plotgg(tt)

tt<-parseJsonTemplate("Frost_fig1",transform_options=c(FeOonly=T))
plotgg(tt)

tt<-parseJsonTemplate("projBiot",template_options=c(idealMins=T))
tt <- addTernaryOrnaments(tt)
plotgg(tt)

### However, using the low-level interface gives immensely more flexibility !
## Adding points
data(atacazo)
data(blatna)

########################
##CAREFUL avoid using GCDkit::accessVar, it ruins ggplot !
ddd <- tibble(SiO2 = runif(50),Na2O = runif(50), K2O = runif(50), MgO = runif(50))
ggplot()+
  geom_point(data=ddd,mapping=aes(x=SiO2,y=Na2O+K2O,colour = MgO))
# Ok
accessVar("atacazo")
ddd <- tibble(SiO2 = runif(50),Na2O = runif(50), K2O = runif(50), MgO = runif(50))
ggplot()+
  geom_point(data=ddd,mapping=aes(x=SiO2,y=Na2O+K2O,colour = MgO))
# fails !
############################

tt<-parseJsonTemplate("TAS")
ttg<-graph_template(tt)
ggplot()+ttg # a blank plot

ggplot()+ttg + theme_gcdkit()

ggplot()+
  ttg+
  theme_gcdkit()+
  geom_point(data=atacazo,aes(x=SiO2,y=Na2O+K2O))
# Of course you need to do your aesthetics yourself in this case...

# But it's worth the effort !
ggplot()+
  geom_point(data=atacazo,mapping=aes(x=SiO2,y=Na2O+K2O,colour = MgO,shape=Symbol))+
  ttg+theme_gcdkit()+scale_shape_identity()

ggplot()+
  geom_point(data=atacazo,mapping=aes(x=SiO2,y=Na2O+K2O,colour = MgO,shape=Symbol))+
  ttg+theme_gcdkit()+scale_shape_identity()+facet_wrap(vars(Volcano))

ggplot()+
  geom_point(data=atacazo,mapping=aes(x=SiO2,y=Na2O+K2O,colour = MgO))+
  theme_gcdkit()+ttg+
  geom_point(data=blatna,aes(x=SiO2,y=Na2O+K2O,colour = MgO),colour="lightgrey")

# It is a it more work with ternary diagrams, though
tpl<-parseJsonTemplate("AFM")
tpl <- addTernaryOrnaments(tpl,ticks=T)

tplg<-graph_template(tpl)
ggplot()+
  geom_point(data=pointCoordinates(tpl)$plottingCoords,aes(x=x.data,y=y.data))+
  tplg

# If you want to rotate the template...
tplR <- rotateTernaryTemplate(tpl,30)

tplRg<-graph_template(tplR)
ggplot()+
  geom_point(data=pointCoordinates(tplR)$plottingCoords,aes(x=x.data,y=y.data))+
  tplRg

# It works for diagrams with built-in rotation:
tpl<-parseJsonTemplate("projBiot")
tpl <- addTernaryOrnaments(tpl,ticks=T)

tplg<-graph_template(tpl)
ggplot()+
  geom_point(data=pointCoordinates(tpl)$plottingCoords,aes(x=x.data,y=y.data))+
  tplg+
  coord_fixed(xlim=c(-2,1))

####### UTILITIES ########
######## Test all diagrams
data("blatna")
accessVar(blatna) #blatna has a complete element list, better for testing...

templ_dir <- system.file("templates",package="graphTemplates")
templ_list <- list.files(templ_dir,recursive=T,include.dirs = F)
idx <- endsWith(templ_list,".json")
templ_list <- templ_list[idx]

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

##gg test
sapply(templ_list,
       function(thediag){
         cat("loading",thediag,"...")
         tt <- parseJsonTemplate(thediag)
         if(class(tt)=="binary"){
           cat("plotting\n==================================\n")
           plotgg(tt)
           cat(thediag,"OK\n")
         }else{
           cat("skipping\n")
         }
         readline(prompt="Press [enter] to continue")

       } )

sapply(templ_list,
       function(thediag){
         cat("loading",thediag,"...")
         tt <- parseJsonTemplate(thediag)
         if(class(tt)=="ternary"){
           cat("plotting\n==================================\n")
          tt<-addTernaryOrnaments(tt)
            plotgg(tt)
           cat(thediag,"OK\n")
         }else{
           cat("skipping\n")
         }

       } )

### TESTING ZONE
tt1<-parseJsonTemplate("Frost_fig1")
plotFigaro(tt1,WR,labels)

tt<-parseJsonTemplate("Frost_fig1",transform_options=c(FeOonly=T))
plotFigaro(tt,WR,labels)

tt<-parseJsonTemplate("projBiot",template_options=c(idealMins=T))
tt <- addTernaryOrnaments(tt)
plotFigaro(tt,WR,labels)

### ggplot
tt<-parseJsonTemplate("projBiot")
tt<-addTernaryOrnaments(tt)
plotgg(tt)
