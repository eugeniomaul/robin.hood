\name{robin.hood-internal}
\alias{print.s_tab_display}
\alias{print.s_summarize_display}
\alias{print.s_model_display}
\title{Internal robin.hood print methods}
\description{
Internal S3 print methods used to format the console output of \code{s_tab},
\code{s_tab1}, \code{s_summarize}, \code{s_regress} and \code{s_logistic}. Not
intended to be called directly.
}
\usage{
\method{print}{s_tab_display}(x, \dots)
\method{print}{s_summarize_display}(x, \dots)
\method{print}{s_model_display}(x, \dots)
}
\arguments{
  \item{x}{an object produced internally by the \code{s_} description or model functions.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\keyword{internal}