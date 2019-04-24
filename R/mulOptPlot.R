mulOptPlot <-
function(M.Opt, ind, th, smooth = "NO") {
   if(is.list(M.Opt) & is.vector(ind) & is.numeric(th)){
      oldpar <- par(no.readonly = TRUE)
      par(mfrow = c(2, 1), mai = c(1, 1, 0.5, 1.5))
      image(M.Opt$vt, M.Opt$vm, log(M.Opt$sg[, , ind]), col =topo.colors(256), main = "Significant Genes", axes = FALSE, xlab = "T", ylab = "m")
      axis(1, M.Opt$vt, M.Opt$vt)
      axis(2, M.Opt$vm, M.Opt$vm)
      box()
      image.plot(zlim = c(min(M.Opt$sg[, , ind]), max(M.Opt$sg[, , ind])), nlevel = 64, legend.only = TRUE, col = topo.colors(256), horizontal = FALSE)
      axis(1, M.Opt$vt, M.Opt$vt)
      axis(2, M.Opt$vm, M.Opt$vm)
      box()
      if (smooth == "NO") {
         tmp_FDR_tmp = M.Opt$FDR
         tmp_FDR_tmp[which(M.Opt$FDR < th)] <- th
         tmp_FDR_tmp[which(M.Opt$FDR >= th)] <- 1
         image(M.Opt$vt, M.Opt$vm, tmp_FDR_tmp[, , ind], col = c("black","white"), main = "FDR", axes = FALSE, xlab = "T", ylab = "m")
         axis(1, M.Opt$vt, M.Opt$vt)
         axis(2, M.Opt$vm, M.Opt$vm)
         box()
         par(oldpar)
      }else{
         tmp_FDR_tmp = M.Opt$FDR
         image(M.Opt$vt, M.Opt$vm, tmp_FDR_tmp[, , ind], col =topo.colors(256), main = "FDR", axes = FALSE, xlab = "T", ylab = "m")
         axis(1, M.Opt$vt, M.Opt$vt)
         axis(2, M.Opt$vm, M.Opt$vm)
         box()
         image.plot(zlim = c(min(tmp_FDR_tmp[, , ind]), max(tmp_FDR_tmp[, , ind])), nlevel = 64, legend.only = TRUE, col =topo.colors(256), horizontal = FALSE)
         par(oldpar)
      }
    }else{
       stop("error in input files", call. = FALSE)
    }
}

