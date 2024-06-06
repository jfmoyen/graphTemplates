
\name{json format to describe diagram templates}
\alias{jsonDiagramFormat}
\title{Recognized json tag in diagram descriptions}

\description{
Diagram templates are defined using json format. json follows a tag:value convention.}

\details{The functions from jsonGraphTemplates understand the following tags. Note, by the way, that json format does NOT permit comments - if you really need to, you can add somehing like "_comment":"blabla" (it will be loaded, but probably dropped by the parser). Fields that the parser can not read will be silently ignored...}

\section{Mandatory}{
  \itemize{
  The following items are mandatory and must be present in the template
  \item{"Rdialect:" }{Indicates which "dialect" the template requires: plain R (\code{base}) or \code{GCDkit} ? }
    \item{"diagramType": }{One of "binary", "ternary" or "plate". Describes the geometry, and overall behviour of the diagram. Plates are a collections of simpler diagram and the parser should be able to deal with them.}
    \item{"axesDefinition": }{Axes mapping. In a binary diagrams, axes are X and Y. In a ternary, apices are A, B and C. This should be either variables from the dataset, or values that can be calculated from them. In GCDkit they are calculated using calcCore, in ggplot using data-masking. In base R, only natural variables can be used; you can always calculate in advance however. Do NOT include corresponding template elements in the file (even for ternary), they are automatically created by the parser.}
    \item{"limits": }{a list of two, X and Y, giving the range on each axis. For a ternary it will be invariably about 0-1}
  }
}

\section{Optional}{

These items can be included to customize further the template
\subsection{metadata}{
\itemize{
\item{"name": }{Short name of the diagram, normally the GCDkit function it originates from}
\item{"fullName": }{Long version of the name, will be printed as diagram title}
\item{"details": }{If needed. Probably a more clever place to put comments if required}
\item{"reference": }{Original publication}
\item{"url": }{Original URL, DOI, etc.}
\item{"templateAuthor": }{Name and contact of the person who wrote the template file (not the original diagram)}
\item{"templateHistory": }{Further comments on the template itself}
}}

\subsection{Data transformation}{
\itemize{
\item{"dataTransform": }{null, or a function of wrdata that returns a data matrix. If null, the current data will be used. Otherwise code{dataTransform(WR)} will be used. If a data transform function exists, it is possible to pass arguments to it using, in the parser, transform_options = list(...) }
\item{"dataTransformParams": }{Parameters to be passed to the data transform function (always). Typically things such as verbose = F ... }
\item{"dataFilter": }{null, or a string corresponding to a boolean filter, used by GCDkit::selectSamples or dplyr::filter for instance}
}}



\subsection{Axes}{
\itemize{
\item{"axesName": }{The name of the axes, as will be printed on the graph (as opposed to their actual definition, i.e. how they will be calculated). Defaults to axesDefinition. They are X and Y for binary, A, B and C for ternary. Do NOT include corresponding template elements in the file, they are automatically created by the parser.}
\item{"suppressAxes": }{If present (and true), the axes will not be drawn. Neded for some special plots where custom axes are required (and should be defined in the template). A clever ternary plotter should
automatically remove them without further ado.}
\item{"log": }{If present (and other than ""), defines which axes use a log scale using base R convention : "x", "y" or "xy"}
}
A clever ternary plotter should be able to plot the ternary axes without asking, so they should NOT
be in the template (it does not matter a lot if they are though, there will be two lines...)
}
}

\section{Template}{
The template proper is a list of potentially many elements. They all belong to a "template": element, itself a dictionary of the form "element_name":"description". Each template element starts with a "type" element that defines how it will be interpreted by the parser. The following types are understood:
\itemize{
\item{"plotFun":"NULL" }{Indicates which plotting function to use. Elements with plotFun "NULL" won't be plotted on the template. They can, however, be used for clasification}
\item{"plotFun":"lines"}{Elements of type lines will draw lines. Lines are defined by x, y, possibly col, lty }
\item{"plotFun":"text" }{Text elements can be defined as plain strings, or as expressions, i.e. both \code{"text": "SiO2"} and \code{"text": "expression({SiO[2]})"  } will work - of course, the latter will be formatted.}
\item{"plotFun":"abline"}
\item{"plotFun":"arrows"}
}


\subsection{Styling}{
Many template elements can be styled by the parser, using a combination of template-level defaults and user-supplied variables.

If a recognized element (see below) is found, the parser will interpret it:
\itemize{
\item a recognized value (e.g. "black" or "#123456" for a colour, "dashed" for a lty, 2.5 for a lwd...) will be used without modification and is immutable.
\item any other string (e.g. "pltcol1") will be interpreted as a variable, which will be replaced by a proper value:
\itemize{
\item if there is a global styling option of that name in the template, by the value of this option;
\item if the user passed an alternative value through styling_options, by the user value. This takes precedence over a template default.
}
}

Sor for instance:
\itemize{
\item a template element defined with "col":"red" will always be interpreted as red.
\item a template element defined with "col":"pltcol1" will be interpreted, in order of preference:
\itemize{
\item if the parser has been called with styling_options=c(pltcol1="blue"), the user option will be used (blue, in this case)
\item is there is a styling default in the template, such as 	"styleDefaults": {"pltcol1": "green"}, it will be used (the element will be green)
\item else, the value of styling_options["defaultCol"]
\item is everything else fails, black.
}
}


Recognized elements are:
\itemize{

\item colors: The field "col" of any template element will be interpreted by the parser
\item lty: The field lty of lines, arrows and abline  will be interpreted.
\item lwd: The field lwd of lines, arrows and abline will be interpreted.
}

Therefore, as much as possible, template designers should use variables (pltcol2) and defaults (	"styleDefaults": \{"pltcol2": "gray8"\}), as they will give the user more control on the representation of the template.
}


\subsection{Switching}{Elements can be controlled by switching. If an element has a "switch" field, this fields contains the name of the (boolean) switch that will control its appearance. An element with no "switch" will always appear.
The list of switches and their defaults MUST be present in "optionSwitches" and "optionDefaults", otherwise the code throws an error (unlike colours, there is no sensible default).

For example:

\code{
"optionSwitches": {"plotthis": "Plot the word Tholeitic",
                    "plotthat": "Plot the diagonal line",
                    "showText":"Show text"}

"optionDefaults": {"plotthis": false,
                   "plotthat": true,
                   "showText":true}
}

By default, all templates should include a switch called showText (controlling the display of text), and it should be used for text elements.

Any option supplied by the user, through template_options, will override the template defaults.

If several switches are given for an element, they ALL need to be true for this element to appear - be it through defaults, or user action.
}
}


\section{classification}{
If an element is named clssf, it is used to define classification polygons, which may, or may not be plotted. clssf must have the following fields:
\itemize{
\item{"use": }{The numbers of the template elements to use for classification, in the order in which they appear in the template.}
\item{"rcname": }{The resulting classification names}
}

Classification requires closed polygons, so any subsequent definition must loop back to origin, like that:

\code{
 "lines1": {"type": "NULL",
            "x": [0,435,0,0],
            "y": [0,500,500,0]
        },
}

This specific "lines" will not be plotted (type is NULL).

As clssf explicitely refers to the item numbers, it would be very unwise to reorder them or remove some of them; in particular do NOT switch them !

For example:
\code{
 "clssf": {"type": "NULL",
            "use": [2,3,4,5,6,7],
            "rcname": ["I","II","III","IV","V","VI"]
        },
  "lines1": { ... },
  "lines2": { ... },
  ...
  "lines6": { ... },
  "lines7": { ... }, (etc)
}

will use elements lines1 to lines6 for classification (but not lines7 and subsequent)
}

\section{Hooks}{
In some cases, the template needs to be modified based on what is found in the data;
for instance, different constants will be used depending on the way Fe is expressed.
This is, hopefully, a rare case. To allow this, the system allows to define a "hook".
A hook is an arbitrary function f(self,wrdata,lbl) where self is the template, wrdata and lbl are the WR and labels. It returns a list with the same components. Meanwhile, the user can modify the template in the required way, for instance add or remove a line based on some value in the data (or even in the labels).

Hooks are executed at plotting time to allow data-dependent changes. If the template needs to be modified before (for instance, assing ternary axes, etc) it is easier to directly manipulate the template itself.
}


\section{Plates}{
Plates are defined as a collection of individual plot, stored in the plateSlot component.
Each plot is itself described in a separate json file, which can therefore be reused from somewhere else,
or conversely be used as a stand-alone template. The global plate definition contains plate-level information, and the name of the sub-plots to draw.
\subsection{Plate-level informations}{
Plate level information includes the usual meta data (name,fullName, etc.). It should also
include \code{nrow} and \code{ncol} to indicate how the plots will be arranged. If this is
missing, the parser will try to arrange the sub-plots in a sensible way.

It may include global options (e.g. \code{"showText": false}, sensible in this context). If the user
uses global options, as for normal template, they must have a full definition with names and defaults:
\code{
    "optionSwitches": {"showText":  "Default option to show diagram text"},
    "optionDefaults": {"showText": false}
}

These options will override subplot-level options but will be overridden by anything user supplied.
}
\subsection{Sub-plots}{

Subplots are defined as a list, containing at least the name of the diagram.

It is also possible to rescale a template to include it into a subplot. This is done by supplying limits,
that will override the built-in ones. If this is done, full limits (X and Y) must be given.
}

A complete slots definition looks like that:
\code{
    "plateSlots":{
      "slot1":{"diagram":"Shand.json",
            "limits": {
        "X": [0.5, 1.5],
        "Y": [0, 3] }
      },
      "slot2":{"diagram":"AFM.json"},
      "slot3":{"diagram":"Shervais.json"}
    }
}
}


