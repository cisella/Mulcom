harmonicMean <-
function( index ){
  harmonic_mean <- rep(NA, length(levels(factor(index)))-1)
  for(i in 1:length(levels(factor(index)))-1){
     harmonic_mean[i] <- (2/((1/length(index[index == 0 ]))+(1/length(index[index == i]))))
  }
  return(harmonic_mean)
}

