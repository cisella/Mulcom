mulInt <- function(...){
   nam <- as.character(match.call(expand.dots = TRUE))[-1L]
   lst <- list(...)
   out <- vector()
   ref <- unique(unlist(lst))
   for(i in 1:length(lst)){
      map <- rep(0, length(ref))
      map[match(lst[[i]], ref, nomatch = 0)] <- 1
      out <- cbind(out, map)
   }
   rownames(out) <- ref
   colnames(out) <- nam
   return(out)
}
