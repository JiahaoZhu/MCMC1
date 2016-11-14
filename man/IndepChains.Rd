\name{IndepChains}
\alias{IndepChains}
\title{
Generate Independent Chains
}
\description{
To generate independent chains from the posterior distribution
}
\usage{
IndepChains(m, n, start, logtarget, sd)
}
\arguments{
  \item{m}{
How many replications do you want, i.e how many independent chains do you want
}
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
The independent chains
}
\examples{
#The log of the posterior distribution
logposter <- function(p){
 if ((p < 0) || (p > 1)) -Inf
 else {
-(p - 0.5) ^ 2 / 0.08 + 54 * log(p) + 146 * log(1-p)
  }
}

#Sample several independent Markov Chains
IndepChains(100, 10000, 1/2, logposter, sd = 1)
}
