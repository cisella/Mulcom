mulDELTA <-
function(vector,index){
   tapply(vector[index>0], factor(index[index>0]), function(vector,ctrl){mean(vector)-mean(ctrl)},vector[index==0])
}

