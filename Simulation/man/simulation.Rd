\name{Simulation}
\alias{simulation}
\title{Simulation}
\usage{
regsim(reps = 1000, n = 10, beta = 1.2, eps.dist = "uniform", ...)
plotRegsim(reps = 1000, n = c(50, 100, 500), beta = c(.4, .8, 1, 1.5),
eps.dist = "uniform", which.plot = "r2", ...)
}

\arguments{
\item{reps}{number of simulated runs}
\item{n}{sample size. A numeric value in \code{regsim}; a vector of length \code{>=} 1 in \code{plotRegsim}.}
\item{beta}{specified true parameter. A numeric value in \code{regsim}; a vector of length \code{>=} 1 in \code{plotRegsim} function.}
\item{eps.dist}{distribution of errors. These must be one of \code{"uniform"}, \code{"t"}, \code{"laplace"}.}
\item{which.plot}{select which plot to display. These must be one of \code{"beta hat"}, \code{"r2"}, \code{"power"}}
\item{...}{additional arguments to pass on for the distribution specified by \code{eps.dist}}

}

\description{
These functions simualtes simple linear regression model for the purpose of examining violation of the assumption that errors are normally distributed. \code{regsim} simulates simple linear regression model with normal and non-normal error distributions. \code{plotRegsim} plots R-squared, estimated beta hats, and power from simulated model with normal and non-normal error distributions.
}

\examples{
regsim(reps = 100, n = 100, beta = 1.2,
       eps.dist = "uniform", min = -1, max = 1)
regsim(reps = 100, n = 100, beta = 1.2,
       eps.dist = "t")
regsim(reps = 100, n = 100, beta = 1.2,
       eps.dist = "laplace", m = 0, s = 1)

# produces plots of R-squares for combinations of sample sizes and betas
plotRegsim(reps = 100, n = c(50, 100, 500), beta = c(.4, .8),
           eps.dist = "uniform", min = -1, max = 1,
           which.plot = "beta hat")

# produces plots of R-squares for combinations of sample sizes and betas
plotRegsim(reps = 100, n = c(50, 100, 500), beta = c(.4, .8, 1, 1.5),
           eps.dist = "laplace", m = 0, s= 1,
           which.plot = "r2")

# produces plots of power versus sample size for a given beta
plotRegsim(reps = 100, n = c(50, 100, 500), beta = c(.4, 1, 1.5),
           eps.dist = "t",
           which.plot = "power")


}
