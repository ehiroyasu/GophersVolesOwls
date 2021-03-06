% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predprey_sim.R
\name{predprey_sim}
\alias{predprey_sim}
\title{Predator Prey Simulation Function}
\usage{
predprey_sim(times, state, r, pred_func)
}
\arguments{
\item{times}{is a numerical sequence of time steps, this should be the same as the t parameter in the pred_prey function}

\item{state}{is a matrix of starting values for N and P}

\item{r}{is a vector of r-values used in the simulation}
}
\description{
A function to simulate the pred_prey function in the owl package for multiple runs. This returns a list of 3D arrays with a
different element for each r-value.  The value of the r-value is the name of each element of the list.
Each element contains a simulation for varying state variables (usually Predator density).
}
\author{
Elizabeth Hiroyasu
}
