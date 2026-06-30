\name{inspect.plot}
\alias{inspect.plot}
\title{
Visually Exploring a Dataframe imported into R (Step 1)
}
\description{
Using this function is the first step to find out the distribution of variables in a dataset and look for outliers.
A pdf file is generated to the working directory by default having 6 graphs per page.

}
\usage{
inspect.plot(data = NULL, include.missing = c("yes", "no"), 
             graphs = NULL, graphs.filename = NULL, 
             page.width = NULL, page.height = NULL, out.label = NULL, ...)
}
\arguments{
  \item{data}{
an object of class data.frame imported into R that you want to explore before beginning analysis. No quotes needed.
}
  \item{include.missing}{
the default "yes" will use missing values as an additional category for categorical variables. Choosing the "no" option for this argument will exclude missing values from plots of categorical variables.
}
  \item{out.label}{Custom label prefix.}
  \item{graphs}{
inidicates the number of graphs to be plotted per page. You may choose a number from 1 to 12. The default if you do not specify the graphs option is 6 graphs per page.
}
  \item{graphs.filename}{
if you provide the argument with a name in quotes, (i.e. graphs.filename = "graphs_example"), then a .pdf file is exported with the plots to the working directory. Otherwise a .pdf file is created to the working directory with the default name "dataframe_plots.pdf".
}
  \item{page.height}{
page height in inches. Default is 9 inches.
}
  \item{page.width}{
page width in inches. Default is 7 inches.
}
  \item{\dots}{Additional plotting arguments.}

}
\value{
A .pdf file is saved to the working directory. By default the .pdf file is called "dataframe_plots.pdf" and has 6 plots per page.

}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{codebook}} To explore a dataframe numerically including missing values.

\code{\link{t1}} To explore or summarize variables in a dataframe without a grouping factor.

\code{\link{t1.by}} To explore or summarize variables in a dataframe across a grouping variable.

\code{\link{t23}} To generate tables with coefficients of univariable and multivariable regression. To generate an appropriate object for plotting the regression results in a forest plot like fashion using the ramu.plot function.

\code{\link{ramu.plot}} To plot the regression result using a forest plot like format using an object created by the t23 function.

\code{\link{cluster.corr}} To calculate Pearson and Spearman Correlation Coefficient in data with Clustering.

}
\examples{
data(drops)
## 1 a simplified version of inspect.plot function
inspect.plot(data = drops)

## 2 exporting the pdf with a specified name and 4 graphs per page
## code not run
## inspect.plot(data = drops, graphs.filename = "drops_explore_plots", graphs= 4)
## will export a file called "drops_explore_plots.pdf" to the working directory with 4 plots per page


}

