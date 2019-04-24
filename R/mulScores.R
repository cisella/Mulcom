mulScores <- function(eset, index){

   if(class(eset) == "ExpressionSet" & is.vector(index) | class(eset) == "matrix" | class(eset) == "data.frame"){

      if(class(eset) == "ExpressionSet"){

         mulcom <- new("MULCOM")
         data <- as.vector(as.matrix(exprs(eset)))

         groups <- index
         ngroups <- length(levels(factor(groups)))

         n <- as.integer((dim(eset))[1])
         m <- as.integer((dim(eset))[2])
         reference <- c( 0 )

         means <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ( ngroups - 1 ) ) ) )
         SS <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ngroups ) ) )
         harmonic_means <- c(seq(0, 0, length=ngroups-1 ) )
         sss2 <- c( seq( 0, 0, length = as.integer((dim(eset))[1]) ) )
         mse <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ( ngroups - 1 ) ) ) )

         out <- .C("Single_SimulationC", as.double(data), as.double(means), as.double(harmonic_means), as.double(SS),  as.double(sss2), as.double(mse), as.integer(n), as.integer(m), as.integer(groups), as.integer( ngroups ), as.integer(reference),PACKAGE = "Mulcom")

         mulcom@FC <- t(data.frame(matrix(out[[2]], ncol=(ngroups-1), byrow=TRUE), row.names=featureNames(eset)))
         mulcom@HM <- matrix(out[[3]], ncol=ngroups-1,byrow=TRUE)
#         sumsqDF <- data.frame(matrix(out[[4]], ncol=ngroups,byrow=TRUE), row.names=featureNames(eset))
#         sss2 <- data.frame(matrix(out[[5]], ncol=1,byrow=TRUE), row.names=featureNames(eset))
         mse <- data.frame(matrix(out[[6]], ncol=ngroups-1,byrow=TRUE), row.names=featureNames(eset))

         mulcom@MSE_Corrected <- t(mse)

         return(mulcom)

      }else{

      if(class(eset) == "data.frame" & is.vector(index)){ eset <- as.matrix(eset)}

         if(class(eset) == "matrix" & is.vector(index) ){

            mulcom <- new("MULCOM")
            data <- as.vector(eset)

            groups <- index
            ngroups <- length(levels(factor(groups)))

            n <- as.integer((dim(eset))[1])
            m <- as.integer((dim(eset))[2])
            reference <- c( 0 )

            means <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ( ngroups - 1 ) ) ) )
            SS <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ngroups ) ) )
            harmonic_means <- c(seq(0, 0, length=ngroups-1 ) )
            sss2 <- c( seq( 0, 0, length = as.integer((dim(eset))[1]) ) )
            mse <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ( ngroups - 1 ) ) ) )

            out <- .C("Single_SimulationC", as.double(data), as.double(means), as.double(harmonic_means), as.double(SS),  as.double(sss2), as.double(mse), as.integer(n), as.integer(m), as.integer(groups), as.integer( ngroups ), as.integer(reference),PACKAGE = "Mulcom")

            mulcom@FC <- t(data.frame(matrix(out[[2]], ncol=(ngroups-1), byrow=TRUE), row.names=rownames(eset)))
            mulcom@HM <- matrix(out[[3]], ncol=ngroups-1,byrow=TRUE)
#            sumsqDF <- data.frame(matrix(out[[4]], ncol=ngroups,byrow=TRUE), row.names=rownames(eset))
#            sss2 <- data.frame(matrix(out[[5]], ncol=1,byrow=TRUE), row.names=rownames(eset))
            mse <- data.frame(matrix(out[[6]], ncol=ngroups-1,byrow=TRUE), row.names=rownames(eset))

            mulcom@MSE_Corrected <- t(mse)

            return(mulcom)

         }

      }

   }else{

         stop("error in input files", call. = FALSE)

   }

}

