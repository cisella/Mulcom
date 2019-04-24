mulOpt <-
function (Mulcom_P, vm, vt){
    if(class(Mulcom_P) == "MULCOM_P" & is.vector(vm) & is.vector(vt)){
    tmp_sg <- array(0, c(nrow = length(vt), ncol = length(vm),
        dim(Mulcom_P@FC)[1]))
    tmp_fsg <- array(0, c(nrow = length(vt), ncol = length(vm),
        dim(Mulcom_P@FC)[1]))
    message("MulCom optimization starts")
    packageStartupMessage("initializing ...", appendLF = TRUE)
    quant <- quantile(1:c(length(vm)*length(vt)), seq(0, 1, 0.1))
    z <- 1
    ind <- quant[z]
    j <- 1
    for (i in 1:length(vm)) {
        for (h in 1:length(vt)) {
            j <- j + 1
            tmp_sg[h, i, ] <- mulCalc(Mulcom_P, vm[i], vt[h])
            tmp_fsg[h, i, ] <- mulFSG(Mulcom_P, vm[i], vt[h])
            if(j > ind){
               packageStartupMessage(paste(names(quant[z]), "\r"), appendLF = FALSE)
               z <- z+1
               ind <- quant[z]
            }
        }
    }
    packageStartupMessage(" done")
    tmp_FDR <- tmp_fsg/tmp_sg
    out <- list(sg = tmp_sg, FDR = tmp_FDR, vm = vm, vt = vt, fsg = tmp_fsg)
    dimnames(out$sg)[[2]] <- vm
    dimnames(out$sg)[[1]] <- vt
    dimnames(out$FDR)[[2]] <- vm
    dimnames(out$FDR)[[1]] <- vt
    return(out)
    }else{
        stop("error in input files", call. = FALSE)
    }
}


