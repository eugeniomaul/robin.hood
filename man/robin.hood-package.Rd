\name{robin.hood-package}
\alias{robin.hood-package}
\alias{robin.hood}
\docType{package}
\title{
Simplify Creation of Baseline Descriptive Tables and Display of Regression Results
}
\description{
 This package was written to facilitate and automate the creation of tables and figures for medical literature publications. Only basic knowledge of R is needed to use this functions.
}
\details{
\tabular{ll}{
Package: \tab robin.hood\cr
Type: \tab Package\cr
Version: \tab 4.0\cr
Date: \tab 2018-10-16\cr
License: \tab GPL (>= 2)\cr
}
Functions:

cluster.corr Allows calculation of Pearson and Spearman correlation coefficient for data with clustering (i.e. both eyes of patients included in the analysis) It is a function based on Rosner and Glynn paper (Estimation of rank correlation for clustered data. Stat Med. 2017 Jun 30;36(14):2163-2186.)

codebook Using this function is the first step to find out the structure and contents of a dataset imported into R. 

inspect.plot To display graphs with the distribution of all variables on the dataset making it easy to detect outliers and deviations from a nomrmal distribution in cotinuous variables. 

t1 will simplify exploratory analysis of dataframes that might contain a mix of numeric, factor, logical and character variables. 

t1.by will do the same as t1, but results are grouped by a specified grouping variable. This function will also compare the distributions of values across groups and provide an additional column with a p value.

Both functions t1 and by.t1 functions are also meant to easily provide you with a table 1 for your paper (i.e. a medical literature paper) where baseline characteristics of subjects are reported.

t23 function in this version will allow you to conduct uni and multivariable regression analysis. This function is meant to easily create tables 2 and 3 (hence the function name is t23) of an average paper that reports regression results in the form of a table. In the current version linear regression and logistic regression as well as their gee equivalents are implemented.

ramu.plot function is the best seller of this package. Great for scientific posters, presentations and also for papers where you prefer a visual representation of your regression results in a forest plot like format.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH

Maintainer: Eugenio A. Maul <emaul@fola.cl>
}

\references{
Juergen Gross and bug fixes by Uwe Ligges (2012). nortest: Tests for Normality. R
  package version 1.0-2. http://CRAN.R-project.org/package=nortest

Vincent J Carey. Ported to R by Thomas Lumley and Brian Ripley. Note that maintainers
  are not available to give advice on using a package they did not author. (2012). gee:
  Generalized Estimation Equation solver. R package version 4.13-18.
  http://CRAN.R-project.org/package=gee

R Core Team (2012). R: A language and environment for statistical computing. R
  Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL
  http://www.R-project.org/.

Liang, K.Y. and Zeger, S.L. (1986) Longitudinal data analysis using generalized linear models.
  Biometrika, 73 13–22.

Zeger, S.L. and Liang, K.Y. (1986) Longitudinal data analysis for discrete and continuous outcomes.
  Biometrics, 42 121–130.

Dallal, G.E. and Wilkinson, L. (1986): An analytic approximation to the distribution of 
  Lilliefors' test for normality. The American Statistician, 40, 294–296.

Patrick Royston (1982) Algorithm AS 181: The W test for Normality.
  Applied Statistics, 31, 176–180.

Rosner B, Glynn RJ. (2017) Estimation of rank correlation for clustered data.
	Stat Med., 36(14):2163-2186.
}
\seealso{
\code{\link{codebook}} To explore a dataframe numerically including missing values.

\code{\link{inspect.plot}} To display graphs with the distribution of all variables on the dataset making it easy to detect outliers and deviations from a nomrmal distribution in cotinuous variables.

\code{\link{t1}} To explore or summarize variables in a dataframe without a grouping factor.

\code{\link{t1.by}} To explore or summarize variables in a dataframe across a grouping variable.

\code{\link{t23}} To generate tables with coefficients of univariable and multivariable regression. To generate an appropriate object for plotting the regression results in a forest plot like fashion using the ramu.plot function.

\code{\link{ramu.plot}} To plot the regression result using a forest plot like format using an object created by the t23 function.

\code{\link{cluster.corr}} To calculate Pearson and Spearman Correlation Coefficient in data with Clustering.

}
\examples{
# See help for each of the functions for examples
}
