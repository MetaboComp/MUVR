% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMV.R
\name{plotMV}
\alias{plotMV}
\title{Plot MV object Swim lane plot for classification, for regression}
\usage{
plotMV(MUVRclassObject, model = "min", factCols, sampLabels, ylim = NULL)
}
\arguments{
\item{MUVRclassObject}{An MUVR class object obtained from the MVWrap function}

\item{model}{What type of model to plot ('min', 'mid' or 'max'). Defaults to 'mid'.}

\item{factCols}{An optional vector with colors for the factor levels (in the same order as the levels)}

\item{sampLabels}{Sample labels (optional; implemented for classification)}

\item{ylim}{Optional for imposing y-limits for regression and classification analysis}
}
\value{
A plot of results from multivariate predictions
}
\description{
Plot MV object Swim lane plot for classification, for regression
}
