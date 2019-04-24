mulIndex <- function (index, np , seed = 7) {
   set.seed(seed)
   quant <- quantile(1:np, seq(0, 1, 0.1))
   h <- 1
   ind <- quant[h]
   out <- vector()
   for (i in 1:np) {
      tmp <- sample(index)
      while (min(tapply(sample(index), as.vector(index), function(x) {length(unique(x))})) == 1) {

         tmp <- sample(index)
      }
      out <- cbind(out, tmp)
    }
    return(out)
}

