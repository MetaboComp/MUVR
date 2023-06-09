% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plsInner.R
\name{plsInner}
\alias{plsInner}
\title{PLS model in inner CV loop}
\usage{
plsInner(xTrain, yTrain, xVal, yVal, DA, fitness, comp = 5, scale = TRUE)
}
\arguments{
\item{xTrain}{Training data (samples as rows; variables as columns)}

\item{yTrain}{Training response}

\item{xVal}{Validation data}

\item{yVal}{Validation response (for tuning)}

\item{DA}{Logical for discriminant analysis (classification)}

\item{fitness}{Fitness function ('MISS', 'AUROC' or 'RMSEP')}

\item{comp}{Max number of components to try}

\item{scale}{Whether or not to scale inData (X)}
}
\value{
An object containing:

(`miss`, `auc` or `rmsep`) A fitness metric

`nComp` Optimised number of components within range (1:comp)

`virank` variable importance rankings
}
\description{
Called from Wrapper
}
