\name{estimateError}
\alias{estimateError}
\title{
Estimate Markov Chain Monte Carlo Error
}
\description{
To estimate Markov Chain Monte Carlo error in the estimate of mean of a function of the parameters
}
\usage{
estimateError(chains,f)
}
\arguments{
  \item{chains}{
The independent chains where you can get different estimates of mean
}
  \item{f}{
The function of parameters whose mean you want to estimate
}
}
\value{
The sample standard deviation of means, i.e the Markov Chain Monte Carlo error
}
\examples{
#The log of the posterior distribution
logposter <- function(p){
 if ((p < 0) || (p > 1)) -Inf
 else {
-(p - 0.5) ^ 2 / 0.08 + 54 * log(p) + 146 * log(1-p)
  }
}

#The function of the parameters
f <- function(x){
   x^2
}

chains <- IndepChains(10, 10000, 1/2, logposter, sd = 1)

#Calculate the Monte Carlo error
estimateError(chains,f)
}
