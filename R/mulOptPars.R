mulOptPars <-
function (opt, ind, ths){
   if(is.list(opt) & is.vector(ind) & is.vector(ths)){
      coo <- c("NA", "NA")
      fdr <- opt$FDR[, , ind]
      sg <- opt$sg[, , ind]
      if (length(which(fdr < ths)) == 0) {
         print("No significant genes with chosen threshold")
         return(coo <- vector())
      }
      fdr[which(fdr >= ths)] <- 1
      sg[which(fdr >= ths)] <- 0
      sigs <- sg[which(fdr < ths)]
      best <- sg[sg == max(sigs)]
      out <- rep(0, length(fdr))
      dim(out) <- dim(fdr)
      out[sg == max(sigs)] <- 1
      inds <- which(out == 1)
      cors <- vector()
      for(i in 1:length(inds)) {
         tm <- vector()
         tm[1] <- inds[i]
         if(inds[i]%%dim(fdr)[1] == 0) {
            tm[2] <- it <- dimnames(fdr)[[1]][dim(fdr)[1]]
            tm[3] <- iv <- dimnames(fdr)[[2]][(inds[i] - inds[i]%%dim(fdr)[1])/dim(fdr)[1]]
         }else{
            tm[2] <- it <- dimnames(fdr)[[1]][inds[i]%%dim(fdr)[1]]
            tm[3] <- iv <- dimnames(fdr)[[2]][((inds[i] - inds[i]%%dim(fdr)[1])/dim(fdr)[1])+1]
         }
         cors <- cbind(cors, tm)
      }
      tms <- cors[, cors[2, ] == max(cors[2, ])]
      if(length(tms) > 3) {
         coo <- vector()
         coo[1] <- as.numeric(tms[2, tms[3, ] == min(tms[3, ])])
         coo[2] <- as.numeric(tms[3, tms[3, ] == min(tms[3, ])])
         coo <- as.numeric(coo)
         names(coo) <- c("t", "m")
         return(coo)
      }else{
         coo[1] <- as.numeric(tms[2])
         coo[2] <- as.numeric(tms[3])
         coo <- as.numeric(coo)
         names(coo) <- c("t", "m")
         return(coo)
      }
   }else{
      stop("error in input files", call. = FALSE)
   }
}




