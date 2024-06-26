% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templateElement_class.R
\name{showTemplateElement}
\alias{showTemplateElement}
\alias{showTemplateElement.templateElement}
\title{Controls the display of individual template elements}
\usage{
showTemplateElement(self, options)

\method{showTemplateElement}{templateElement}(self, options)
}
\arguments{
\item{self}{a template element}

\item{options}{a named boolean vector containing the values of the switches.}
}
\description{
This method can be used to remove template elements based on switches.
If ALL the switches are TRUE, or is the element has no switch, the element is kept,
Otherwise, if ANY of the switches is FALSE,  the element is removed from
the template - i.e. the method returns NULL
}
