setClass(
   "MULCOM", 
   representation(
      FC="matrix", MSE_Corrected="array", HM="matrix")
    , prototype=list(FC=matrix(0,0,0), MSE_Corrected=matrix(0,0,0), HM=matrix(0))
)

