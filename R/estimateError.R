estimateError <- function(chains,f){
  m <- length(chains[1,1,])
  l <- length(chains[1,,1])
  means <- numeric(m)
  for(i in 1:m){
    target.f <- f(chains[,seq(1,l),i])
    means[i] <- estimateMean(target.f)
  }
  cat("The Monte Carlo error is", sd(means) / sqrt(m), "\n")
}
