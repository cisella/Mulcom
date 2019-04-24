mulCAND <- function (eset, Mulcom_P, m, t, ese = "T") {
    if (class(eset) == "ExpressionSet" & class(Mulcom_P) == "MULCOM_P" & is.numeric(m) & is.numeric(t) | class(eset) == "matrix") {
        if (ese == "T") {
            h <- abs(Mulcom_P@FC) - (Mulcom_P@MSE_Corrected * t) 
            d <- h
            d[which(h < m)] <- 0
            d[-which(h < m)] <- 1
            tmp <- apply(d, 2, max)
            test <- d[, which(tmp == "1")]
            out <- eset[which(tmp == "1"), ]
            return(list(eset = out, dunnett = test))
        }  
        else {
            h <- abs(Mulcom_P@FC) - (Mulcom_P@MSE_Corrected * t)
            d <- h
            d[which(h < m)] <- 0
            d[-which(h < m)] <- 1
            tmp = apply(d, 2, max)
            if (class(eset) == "ExpressionSet") {
                out <- featureNames(eset[which(tmp == "1"), ])
            }
            else {
                out <- rownames(eset[which(tmp == "1"), ])
            }
        }
    }
    else {
        stop("error in input files", call. = FALSE)
    }
}





