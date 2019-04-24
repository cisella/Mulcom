mulParOpt <- function (perm, M.Opt, ind, th, image = "T") {
    if (is.list(M.Opt) & is.vector(ind) & is.numeric(th)) {
        coo <- mulOptPars(M.Opt, ind, th)
        if(is.numeric(coo[1])){
            if (image == "T") {
               sg <- mulCalc(perm, m = coo[2], t = coo[1])
               print(sg[ind])
               oldpar <- par(no.readonly = TRUE)
               par(mai = c(1, 1, 0.5, 1.5))
               tmp_sg_tmp = M.Opt$sg[, , ind]
               tmp_sg_tmp[which(M.Opt$FDR[, , ind] >= th)] <- sg[ind]
               dim(tmp_sg_tmp) <- dim(M.Opt$sg[, , ind])
               image(M.Opt$vt, M.Opt$vm, tmp_sg_tmp, col = topo.colors(256),
                   main = "Mulcom Optimization", axes = FALSE)
               axis(1, M.Opt$vt, M.Opt$vt)
               axis(2, M.Opt$vm, M.Opt$vm)
               box()
               par(new = T)
               tmp_FDR_tmp <- M.Opt$FDR[, , ind]
               tmp_FDR_tmp[which(M.Opt$FDR[, , ind] >= th)] <- 1
               tmp_FDR_tmp[which(M.Opt$FDR[, , ind] < th)] <- NaN
               image(M.Opt$vt, M.Opt$vm, tmp_FDR_tmp, col = c("darkgrey",
                   "white"), axes = FALSE,
                   xlab = "t", ylab = "m", cex= 1.5)
               axis(1, M.Opt$vt, M.Opt$vt)
               axis(2, M.Opt$vm, M.Opt$vm)
               box()
               image.plot(zlim = c(min(tmp_sg_tmp), max(tmp_sg_tmp)),
                   nlevel = 64, legend.only = TRUE, col = topo.colors(256),
                   horizontal = FALSE, 
                   legend.args=list( text= "number of genes", cex=1.5, side=4, line=3))
               mtext("y=1/x", side=4, line=3, cex.lab=1,las=2, col="blue")
               par(oldpar)
           }
           return(coo)
        }
        return(coo)
    }
    else {
        stop("error in input files", call. = FALSE)
    }
}


