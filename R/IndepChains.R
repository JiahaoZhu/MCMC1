IndepChains <- function(m,n,start,logtarget,sd){
  l <- length(start)
  chains <- array(NA, dim = c(n, l, m))
  for (i in 1:m)
    chains[,,i] <- MetroRandom(n, start, logtarget, sd)
  chains
}
