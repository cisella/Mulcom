mulDiff <- function (eset, Mulcom_P, m, t, ind) {
    if (class(eset) == "ExpressionSet" & class(Mulcom_P) == "MULCOM_P" & is.numeric(m) & is.numeric(t) | class(eset) == "matrix" | class(eset) == "data.frame" ) {

            tmp <- mulCAND(eset, Mulcom_P, m, t)

            if(class(eset) == "ExpressionSet"){

               out <- featureNames(tmp$eset)[which(tmp$dunnett[ind,] > 0)]
               return(out)

            }else{

               if(class(eset) == "data.frame" | class(eset) == "matrix"){

                  out <- rownames(tmp$eset)[which(tmp$dunnett[ind,] > 0)]
                  return(out)

               }

            }
            
    }

    else {

        stop("error in input files", call. = FALSE)

    }
}



