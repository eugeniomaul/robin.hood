\name{s_export}
\alias{s_export}
\title{
Export Several Saved Results to a Single csv File
}
\description{
Takes any number of objects saved from \code{\link{s_tab}}, \code{\link{s_tab1}}
and \code{\link{s_summarize}} and writes them, one below the other, into a single
csv file. Each block is preceded by a title row and followed by a blank row, so
that all the tables from an analysis can be reviewed or pasted into a manuscript
from one sheet.
}
\usage{
s_export(..., file = "s_export", sep = ",", verbose = TRUE)
}
\arguments{
  \item{\dots}{one or more objects created by \code{\link{s_tab}},
  \code{\link{s_tab1}} or \code{\link{s_summarize}} (that is, objects of class
  \code{s_describe}). They are written in the order given. An object containing
  several tables (for example a cross-tabulation of one variable against several
  others, or results split by a grouping variable) contributes all of its tables.}
  \item{file}{the base name for the exported file, without extension. The current
  date is appended to avoid inadvertently overwriting earlier exports. Defaults to
  \code{"s_export"}, producing a file such as \code{s_export_2026_07_23.csv} in the
  working directory.}
  \item{sep}{separator for the csv file. If you want a file easily read by Excel
  in a Spanish-language installation, use \code{sep = ";"}. Default \code{","}.}
  \item{verbose}{if TRUE (the default) a message confirms how many objects were
  written and the name of the file created.}
}
\value{
Returned invisibly: the character matrix that was written to the file, with all
blocks stacked vertically and padded to a common number of columns. The file
itself is the main product.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_tab}} For cross-tabulations and one-way frequency tables.

\code{\link{s_tab1}} For one-way frequency tables of several variables.

\code{\link{s_summarize}} For summary statistics of numeric variables.
}
\examples{
data(drops)

## 1 Save some results, then export them together
tab.1  <- s_tab1(drops, c(sex, eye), verbose = FALSE)
tab.2  <- s_tab(drops, c(sex, eye), chi2 = TRUE, verbose = FALSE)
summ.1 <- s_summarize(drops, c(age, md), detail = 2, verbose = FALSE)

## code not run
## s_export(tab.1, tab.2, summ.1, file = "glaucoma_results")

## 2 Same export using a separator suitable for Excel in Spanish
## code not run
## s_export(tab.1, tab.2, summ.1, file = "glaucoma_results", sep = ";")
}