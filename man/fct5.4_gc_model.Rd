% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct5.4_gc_model.R
\name{fct5.4_gc_model}
\alias{fct5.4_gc_model}
\title{G-Computation analysis}
\usage{
fct5.4_gc_model(
  dd,
  confounders,
  fu_time,
  fu_event,
  arm,
  n_iter_gc = 100,
  effect_gc = "ATE"
)
}
\arguments{
\item{dd}{a data frame}

\item{confounders}{a list of confounding factors}

\item{fu_time}{the follow-up time variable for right censored data}

\item{fu_event}{the status event indicator variable: 0 = alive/1 = dead, or T=death/F=alive or 1=alive/2=death}

\item{arm}{the 0/1 binary treatment arm variable}

\item{n_iter_gc}{the number of bootstrap resamples (100 by default) to estimate the variances and the confidence intervals}

\item{effect_gc}{the type of marginal effect to be estimated: could be "ATE" (by default), "ATT" or "ATU" (see details in gc.survival from RISCA package)}
}
\value{
the estimate of the marginal treatment effect from GC analysis
}
\description{
G-Computation analysis
}
\examples{
#to be completed

}
