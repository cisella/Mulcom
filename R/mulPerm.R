mulPerm <- function(eset, index, np, seed, segm = "F"){
   if (class(eset) == "ExpressionSet" & is.vector(index) & is.numeric(np) & is.numeric(seed) | class(eset) == "matrix" | class(eset) == "data.frame") {
#      if(class(eset) == "ExpressionSet"){
          mul <- new("MULCOM_P")
          multest <- mulScores(eset, index)
          mul <- new("MULCOM_P")
          mul@FC <- multest@FC
          mul@MSE_Corrected <- multest@MSE_Corrected
          set.seed(seed)
          fc <- vector()
          mse <- vector()
          ngroups <- length(levels(factor(index)))
          means <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ( ngroups - 1 ) * np ) ) )
          mse <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ( ngroups - 1 ) * np ) ) )
          n <- as.integer((dim(eset))[1])
          m <- as.integer((dim(eset))[2])
          reference <- c(0)
          rand_ind <- mulIndex(index, np = np, seed = seed)
          if(segm == "F"){
             mul_out <- mulPermC(eset, rand_ind, means, mse, n, m, nump=np, ngroups, reference)
             mul@FCp <- as.matrix(mul_out[[3]])
             dim(mul@FCp) <- c(c(ngroups -1),dim(eset)[1],np)
             mul@MSE_Correctedp <- as.matrix(mul_out[[4]])
             dim(mul@MSE_Correctedp) <- c(c(ngroups -1),dim(eset)[1],np)
             return(mul)
          }else{
             npst <- np
             np <- vector()
             stps <- seq(1, npst, by=segm)
             lst <- list();
             for(i in 1:c(length(stps)-1)){
                lst[[i]] <- stps[i]:c(stps[c(i+1)]-1)
             }
             lst[[i+1]]<-stps[c(i+1)]:npst
             fcs <- vector()
             mses <- vector()
             for(i in 1:length(lst)){
                rand_tmp <- rand_ind[,lst[[i]]]
                nump <- length(lst[[i]])
                means <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ( ngroups - 1 ) * nump ) ) )
                mse <- c( seq ( 0, 0, length = ( (dim(eset))[1] * ( ngroups - 1 ) * nump ) ) )
                mul_out <- mulPermC(eset, rand_tmp, means, mse, n, m, nump, ngroups, reference)
                fcs <- c(fcs,as.matrix(mul_out[[3]]))
                mses <- c(mses,as.matrix(mul_out[[4]]))
             }
             np <- npst
             mul@FCp <- as.array(fcs)
             dim(mul@FCp) <- c(c(ngroups -1),dim(eset)[1],np)
             mul@MSE_Correctedp <- as.array(mses)
             dim(mul@MSE_Correctedp) <- c(c(ngroups -1),dim(eset)[1],np)
             return(mul)
          }
#      }else{
#         print("hello")
#      }
   }else{

      stop("error in input files", call. = FALSE)

   }
}


