% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templateElement_class.R
\name{styleTemplateElement}
\alias{styleTemplateElement}
\alias{styleTemplateElement.templateElement}
\title{Controls the styling of individual template elements}
\usage{
styleTemplateElement(self, options)

\method{styleTemplateElement}{templateElement}(self, options)
}
\arguments{
\item{self}{a template element}

\item{options}{a named vector containing the values of the optional colours, ltys or lwds.
It should contain values for "colDefault", "ltyDefault" and "lwdDefault"
(although there are safeguards)}
}
\value{
A templateElement, with col, lty and lwd values added
or modified according to specs
}
\description{
This method can be used to change various graphical parameters
controlling the aspect of template graphical elements. For the moment,
only color can be changed.
}
\details{
Styling of col, lwd and lty works similarly. In all cases, the logic is the following:
\itemize{
\item If there is no field of that name, it will be added and given the default value,
supplied in options (or defaulting to "black", "solid" and 1).
\item If there is a hard-coded value (e.g. a well-conformed color name, a string such
as "dashed", etc.), it will be kept.
\item If there is a variable defined value (e.g. "plotco1"), it will be replaced,
in order of preference:
\itemize{
\item By the user-supplied calue for it, in the form options=c(pltcol="blue", ... )
\item By the default defined in the json template
\item By the default, that should come in options as options = c(colDefault="white");
or by the "default default" (black, solid and 1).
}
}
Any element that has col, lty or lwd will be processed.
}
