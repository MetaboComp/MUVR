% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkinput.R
\name{checkinput}
\alias{checkinput}
\title{This can be run to test if the command input of parameters contradict to each other
And also check the structure of the data
If something goes wrong warning messages are given}
\usage{
checkinput(
  X,
  Y,
  ML,
  DA,
  method,
  fitness,
  nInner,
  nOuter,
  varRatio,
  scale,
  modReturn,
  logg,
  parallel
)
}
\arguments{
\item{X}{The original data of X, not the result after onehotencoding}

\item{Y}{The original data of Y}

\item{ML}{ML in MUVR}

\item{DA}{DA in MUVR}

\item{method}{RF or PLS so far in MUVR}

\item{fitness}{fitness in MUVR}

\item{nInner}{nInnerin MUVR}

\item{nOuter}{nOuter in MUVR}

\item{varRatio}{varRatio in MUVR}

\item{scale}{scale}

\item{modReturn}{modReturn in MUVR}

\item{logg}{logg in MUVR}

\item{parallel}{parallel in MUVR}
}
\value{
correct_input: the original input(call) and the real input used in MUVR when you enter your input
}
\description{
This can be run to test if the command input of parameters contradict to each other
And also check the structure of the data
If something goes wrong warning messages are given
}
