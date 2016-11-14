\name{estimateMean}
\alias{estimateMean}
\title{
Estimate the Posterior Mean of a Function of the Parameters
}
\description{
To estimate the posterior mean of a function of the parameters
}
\usage{
estimateMean(f)
}
\arguments{
  \item{f}{
The function of the parameters
}
}

\value{
The estimated posterior mean of the function of the parameters
}

\examples{
#The log of the posterior distribution
logposter <- function(p){
 if ((p < 0) || (p > 1)) -Inf
 else {
-(p - 0.5) ^ 2 / 0.08 + 54 * log(p) + 146 * log(1-p)
  }
}

post.estimates <- MetroRandom(10000, 1/2, logposter, sd = 1)

#The function of the parameters
f <- function(x){
   x ^ 2
}

#Calculate the mean
estimateMean(f(post.estimates))
}

