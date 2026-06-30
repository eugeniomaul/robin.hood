\name{ica.plot}
\alias{ica.plot}
\title{Multiple Regression Forest Plot}
\description{Plots the coefficients for a single predictor variable across multiple t23 regression objects.}
\usage{
ica.plot(models.list, x.varname, y.varnames = NULL, x.level.names = NULL, 
         exp = FALSE, plot.pdf = FALSE, no.screen = FALSE, 
         plot.name = "ica_plot", plot.height = 9, plot.width = 8, 
         xlim.coord = NULL, x.ticks = NULL, xaxis.title = NULL, 
         h.coeff.align = NULL, v.coeff.align = -0.3, 
         coeff.labels = c("name.p", "name.coeff.p", "name", 
         "full.1.line", "full.2.line"), signif.col = "red", 
         non.signif.col = "navyblue", right.censor = NULL, 
         left.censor = NULL, main.text = "", axis.title.mag = 1.5, 
         axis.label.mag = 1.5, axis.line.width = 1.5, point.mag = 2, 
         point.type = 18, ci.line.width = 1.5, coeff.labels.mag = 1.25, 
         vertical.line.width = 2, plot.margins = c(8.1, 2.1, 4.1, 2.1), ...)
}
\arguments{
  \item{models.list}{A list of objects created by the t23 function.}
  \item{x.varname}{The predictor variable name to extract.}
  \item{y.varnames}{Optional custom names for the outcome variable headers.}
  \item{x.level.names}{Optional character vector to rename factor levels.}
  \item{exp}{Logical indicating if coefficients should be plotted exponentiated.}
  \item{plot.pdf}{Logical to export PDF.}
  \item{no.screen}{Logical to suppress screen plotting.}
  \item{plot.name}{Filename for the PDF.}
  \item{plot.height}{PDF height.}
  \item{plot.width}{PDF width.}
  \item{xlim.coord}{X-axis limits.}
  \item{x.ticks}{X-axis ticks.}
  \item{xaxis.title}{X-axis title.}
  \item{h.coeff.align}{Horizontal label alignment.}
  \item{v.coeff.align}{Vertical label alignment offset.}
  \item{coeff.labels}{Type of label displayed.}
  \item{signif.col}{Color for p<=0.05.}
  \item{non.signif.col}{Color for p>0.05.}
  \item{right.censor}{Truncate CI lines above this value.}
  \item{left.censor}{Truncate CI lines below this value.}
  \item{main.text}{Plot main title.}
  \item{axis.title.mag}{Title magnification.}
  \item{axis.label.mag}{Axis label magnification.}
  \item{axis.line.width}{Line width for axes.}
  \item{point.mag}{Size of the estimate point.}
  \item{point.type}{Type of point symbol.}
  \item{ci.line.width}{Line width for CI.}
  \item{coeff.labels.mag}{Label magnification.}
  \item{vertical.line.width}{Line width for the null vertical line.}
  \item{plot.margins}{Margins around the plot.}
  \item{\dots}{Additional arguments.}
}
