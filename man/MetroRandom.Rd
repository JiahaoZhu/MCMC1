\name{MetroRandom}
\alias{MetroRandom}
\title{
Get Parameters from Posterior Distribution
}
\description{
To simulate parameters from posterior distribution
}
\usage{
MetroRandom(n, start, logtarget, sd)
}

\arguments{
  \item{n}{
The sample size, i.e how many values of the parameters you want from the posterior distribution
}
  \item{start}{
Where do you want to start to simulate your parameters
}
  \item{logtarget}{
The log of your posterior distribution of your parameters
}
  \item{sd}{
The length of the step of your simulation
}
}

\value{
The samples of the parameters from the posterior distribution
}

\examples{
#The log of the posterior distribution
logposter <- function(p){
 if ((p < 0) || (p > 1)) -Inf
 else {
-(p - 0.5) ^ 2 / 0.08 + 54 * log(p) + 146 * log(1-p)
  }
}

#Sample from the Markov Chain
MetroRandom(10000, 1/2, logposter, sd = 1)
}
