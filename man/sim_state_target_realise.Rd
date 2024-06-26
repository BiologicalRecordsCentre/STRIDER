% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_state_target_realise.R
\name{sim_state_target_realise}
\alias{sim_state_target_realise}
\title{Realizes the state_target into binary or abundance using a custom function}
\usage{
sim_state_target_realise(simulation_object, fun, filename = NULL, ...)
}
\arguments{
\item{simulation_object}{a SimulationObject}

\item{fun}{either 'uniform' to use the included uniform suitability function or a function that takes an SimulationObject with an environment slot and outputs a SimulationObject with a target suitability SpatRaster with either a presence/absence or an abundance}

\item{filename}{a file name and path to save the spatraster}

\item{...}{other parameters for the user supplied function fun}
}
\value{
A SimulationObject with a state_target_realised
}
\description{
Realizes the state_target into binary or abundance using a custom function
}
\examples{
\dontrun{
sim_state_target_realise(simulation_object, fun, ...)
}
}
