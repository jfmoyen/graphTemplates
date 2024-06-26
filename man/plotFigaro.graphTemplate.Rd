% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphTemplate_figaro_methods.R
\name{plotFigaro}
\alias{plotFigaro}
\alias{plotFigaro.graphTemplate}
\alias{plotFigaro.binary}
\alias{plotFigaro.ternary}
\alias{plotFigaro.plate}
\title{plotFigaro}
\usage{
plotFigaro(self, wrdata, lbl, new, ...)

\method{plotFigaro}{graphTemplate}(self, wrdata, lbl, new = F, ...)

\method{plotFigaro}{binary}(self, wrdata = WR, lbl = get("labels", .GlobalEnv), new = F, ...)

\method{plotFigaro}{ternary}(self, wrdata = WR, lbl = get("labels", .GlobalEnv), new = F, ...)

\method{plotFigaro}{plate}(self, wrdata = WR, lbl = get("labels", .GlobalEnv), new = F, ...)
}
\arguments{
\item{self}{a graphTemplate object}

\item{wrdata}{a MATRIX containing WR analyses, as per GCDkit's convention.
Probably WR (a global variable) in GCDkit context.}

\item{lbl}{a data.frame containing labels, as per GCDkit's convention.
Probably labels (a global variable) in GCDkit context.}

\item{new}{boolean. If true, open a new graphic window.}

\item{...}{just in case}
}
\description{
Plot a template in Figaro context
}
\details{
Figaro is the internal plotting system of GCDkit.
This method translated a template object, of class graphTemplate, into
a Figaro template (very similar) and invokes Figaro's plotting function.

Figaro is not written using classes (so there is no Figaro class). Also, it operates
on global variables (x.data, y.data, sheet...), so expect LOTS of side effects
when using this method. Of course, the most visible side-effect is plotting
a new graph...

Obviously, this will not work if GCDkit is not present...

Different methods are supplied for binary and ternary graphTemplate.

Plates are yet another animal. GCDkit does a lot of things behind the scenes
when plotting a plate. Some of the arguments can be passed to the undrlying function
(via ...); the interested are aventurous user will note that the function
called to setup plates is .plateSetup = function (number, nrow = NULL, ncol = NULL,
title = NULL, new = TRUE, device = "windows", filename = NULL, colormodel = "rgb").
In RStudio, \code{device=""} can be used to send the plate to the internal viewer.
}
