mulFSG <-
function(Mulcom_P, m, t){
   if(class(Mulcom_P) == "MULCOM_P" & is.numeric(m) & is.numeric(t)){
   d <- abs(Mulcom_P@FCp) - Mulcom_P@MSE_Correctedp * t
   b <- rep(0, dim(d)[1]*dim(d)[2]*dim(d)[3])
   dim(b) <- dim(d)
   b[which(d > m)] <- 1
   fp <- vector()
   for(i in 1:dim(d)[3]){
     out <- apply(b[,,i],1,sum)
     fp <- cbind(fp, out)
   }
   FDR <- apply(fp,1,median)
   return(FDR)
   }else{
      stop("error in input files", call. = FALSE)
   }
}

