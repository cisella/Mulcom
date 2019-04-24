mulCalc <-
function(Mulcom_P, m, t){
   if(class(Mulcom_P) == "MULCOM" | class(Mulcom_P) == "MULCOM_P"){
   if(is.numeric(m) & is.numeric(t)){
      d <- abs(Mulcom_P@FC) - (Mulcom_P@MSE_Corrected*t)
      b <- rep(0, dim(d)[1]*dim(d)[2])
      dim(b) <- dim(d)
      b[which(d > m)] <- 1
      v <- apply(b,1,sum)
      return(v)
   }else{
      stop("error in input files", call.=FALSE)
   }
   }else{
      stop("error in input files", call.=FALSE)
   }
}

