% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotPerm.R
\name{plotPerm}
\alias{plotPerm}
\title{Plot for comparison of actual model fitness vs permutation}
\usage{
plotPerm(
  actual,
  distribution,
  xlab = NULL,
  side = c("greater", "smaller"),
  type = "t",
  xlim,
  ylim = NULL,
  breaks = "Sturges",
  pos,
  main = NULL,
  permutation_visual = "none",
  curve = F,
  extend = 0.1
)
}
\arguments{
\item{actual}{Actual model fitness (e.g. Q2, AUROC or number of misclassifications)}

\item{distribution}{Null hypothesis (permutation) distribution of similar metric as `actual`}

\item{xlab}{Label for x-axis (e.g. 'Q2 using real value',"Q2 using distributions","BER" 'AUROC', or 'Misclassifications')}

\item{side}{Cumulative p either "greater" or "smaller" than H0 distribution (defaults to side of median(H0))}

\item{type}{c('t','non',"smooth","rank","ecdf")}

\item{xlim}{Choice of user-specified x-limits (if default is not adequate)}

\item{ylim}{Choice of user-specified y-limits (if default is not adequate)}

\item{breaks}{Choice of user-specified histogram breaks (if default is not adequate)}

\item{pos}{Choice of position of p-value label (if default is not adequate)}

\item{main}{Choice of user-specified plot title}

\item{permutation_visual}{choice of showing median or mean or none}

\item{curve}{if add curve or not base on the mid}

\item{extend}{how many percenrtage of the orignical range do we start}
}
\value{
Plot
}
\description{
Plots histogram of null hypothesis (permutation) distribution, actual model fitness and cumulative p-value.
Plot defaults to "greater than" or "smaller than" tests and cumulative probability in Student's t-distribution
}
