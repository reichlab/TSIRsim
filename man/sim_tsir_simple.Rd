% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_tsir.R
\name{sim_tsir_simple}
\alias{sim_tsir_simple}
\title{Simple SIR simulation}
\usage{
sim_tsir_simple(pop, r.init, z.init, beta, under.obs, ntimes = 52)
}
\arguments{
\item{pop}{integer, population}

\item{r.init}{initial value for R state}

\item{z.init}{initial value for Z (infected) state}

\item{beta}{transmission parameter in model}

\item{under.obs}{probability of an individual in Z being observed}

\item{ntimes}{number of times to simulate}
}
\value{
A list with the following items
\itemize{
 \item{"obs"}{observed time series of infected Z class}
 \item{"pop"}{The assumed fixed population size}
 \item{"r.init"}{The assumed initial value for the R class}
 \item{"z.init"}{The assumed initial value for the Z class}
 \item{"beta"}{The transmission parameter}
 \item{"under.obs"}{The assumed parameter for reporting probability}
}
}
\description{
Simple SIR simulation
}
\examples{
  sample.z = sim_tsir_simple(3.5e6, 0, 10, 1.5, 0.2)
  plot(sample.z$obs)
  sum(sample.z$obs)/sample.z$pop
}

