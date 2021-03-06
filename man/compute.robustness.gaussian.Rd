% Generated by roxygen2 (4.0.2): do not edit by hand
\name{compute.robustness.guassian}
\alias{compute.robustness.guassian}
\title{Computes robustness under well-characterized compute.robustness (i.e., Gaussian noise).}
\usage{
compute.robustness.gaussian(data, sd, nsamples, problem, method = "default",
  verbose = TRUE)
}
\arguments{
\item{data}{the data set}

\item{sd}{scalar or vector specifying the standard deviation for each
decision variable}

\item{nsamples}{the number of samples to generate for each point}

\item{problem}{the problem formulation}

\item{method}{the robustness metric or a list of metrics to use (see
\code{\link{check.robustness}} for available options)}

\item{verbose}{display additional information}
}
\description{
Adds Gaussian noise to the decision variables and resamples the model output.
Then computes one or more robustness metrics.
}
\details{
This method is equivalent to \code{\link{mordm.compute.robustness}} using a single
model.
}

