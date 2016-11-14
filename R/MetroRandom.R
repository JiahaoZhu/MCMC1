MetroRandom <- function(n, start, logtarget, sd) {
  dim <- length(start)
  result <- matrix(NA, n, dim)
  x <- start; xlogden <- logtarget(x); accepted <- 0
  for (i in seq_len(n)) {
    ystar <- rnorm(dim, x, sd)
    ylogden <- logtarget(ystar)
    u <- runif(1)
    if (log(u) < ylogden - xlogden) {
      result[i,] <- ystar
      x <- ystar; xlogden <- ylogden; accepted <- accepted + 1
    } else {
      result[i,] <- x
    }
  }
  cat("Acceptance rate:  ", round(accepted/n, 2), "\n")
  result
}
