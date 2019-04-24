mulPermC <- function (eset, index, means, mse, n, m, nump, ngroups, reference) {

    if (class(eset) == "ExpressionSet") {

       data <- as.vector(as.matrix(exprs(eset)))

    }else{
       if(class(eset) == "matrix" | class(eset) == "data.frame")
       data <- as.vector(as.matrix(eset))

    }
    
    index_random <- as.vector(as.matrix(index))
    out <- .C("Complete_SimulationC", as.double(data), as.integer(index), 

    as.double(means), as.double(mse), as.integer(n), 
    as.integer(m), as.integer(nump), as.integer(ngroups), 
    as.integer(reference), PACKAGE = "Mulcom")
 
    return(out)

}



