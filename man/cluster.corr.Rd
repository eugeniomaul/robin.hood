\name{cluster.corr}
\alias{cluster.corr}
\title{
Calculating Pearson and Spearman Correlation for Clustered Data
}
\description{
This function will allow users to calculate pearson and spearman correlation coefficients for data with clustering. (i.e. Right and Left Eyes from individuals in the dataset) It requires that you provide a dataframe with at least the id variable (indicating the subject), repeated variable (indicating the identifier for within subject measures. i.e. eye right/left), x variable (the independent variable of interest), y variable (the dependent variable of interest).
Optionally you can specify and adjust the coefficients for covariates (partial correlation coefficients)
Only individuals with complete data for repeated measures are included in the analysis. (i.e. complete data for right and left eyes)
}
\usage{
cluster.corr(data, id.var = NA, rep.var = NA, x.var = NA, 
             y.var = NA, covar.list = NA, n.reps = 2, n.digits = 2)
}
\arguments{
  \item{data}{
an object of class data.frame containing the variables needed for your analysis.
}
  \item{id.var}{
variable identifying clustering level (i.e. subjects or patients) It can be a character, factor or a number. It must be specified in quotes (i.e. id.var="patient.id") 
}
  \item{rep.var}{
variable indicating the identifier for within subject repeated measure. It can be character, factor or numeric. Only subjects with complete data for all the levels of repeated measures are included. (i.e. right and left eye data)
It must be specified in quotes (i.e. rep.var="eye")
}
  \item{x.var}{
numeric variable indicating the independent variable (or predictor) you wish to correlate.
It must be specified in quotes (i.e. x.var="xx")
}
  \item{y.var}{
numeric variable indicating the dependent variable (or outcome) you wish to correlate.
It must be specified in quotes (i.e. y.var="yy")
}
  \item{covar.list}{
optional argument. Vector of names in quotes separated by commas indicating covariates by which you wish to adjust the results. They should be either numeric or factor variables. 
(i.e. covar.list=c("age","gender")
}
  \item{n.reps}{
number of levels for the repeated variable, typically n.reps=2, indicating that the rep.var has 2 levels (i.e. right and left eyes) The default value is 2.
}
  \item{n.digits}{
default value is 2. Indicates the number of decimals you wish yous results to have. The only exception is p values that have an extra 2 decimal places.
}
}
\value{
A list with 3 elements is generated
  \item{acknowledgements}{Character Vector thanking the authors that supplied the SAS code adapted to R in this function}
  \item{pearson.correlation}{Matrix with results for pearson correlation}
  \item{spearman.correlation}{Matrix with results for spearman rank correlation}
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{codebook}} To explore a dataframe numerically including missing values.

\code{\link{inspect.plot}} To display graphs with the distribution of all variables on the dataset making it easy to detect outliers and deviations from a nomrmal distribution in cotinuous variables.

\code{\link{t1}} To explore or summarize variables in a dataframe without a grouping factor.

\code{\link{t1.by}} To explore or summarize variables in a dataframe across a grouping variable.

\code{\link{t23}} To generate tables with coefficients of univariable and multivariable regression. To generate an appropriate object for plotting the regression results in a forest plot like fashion using the ramu.plot function.

\code{\link{ramu.plot}} To plot the regression result using a forest plot like format using an object created by the t23 function.

}
\examples{
data(pigmentosa)
## 1 calculate pearson and spearman correlation coefficient without adjustment for covariates
corr.result1 <- cluster.corr(pigmentosa, id.var="id", rep.var="rep", 
x.var="xx", y.var="yy", n.reps=2, n.digits=2)

## 2 calculate pearson and spearman correlation coefficient with adjustment for covariates covar_a and covar_b
corr.result2 <- cluster.corr(pigmentosa, id.var="id", rep.var="rep", 
x.var="xx", y.var="yy", covar.list=c("covar_a","covar_b"), n.reps=2, n.digits=2)

}

