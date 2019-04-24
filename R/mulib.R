.First.lib <- function(lib,pkg)
{
   library.dynam("Mulcom",pkg,lib)
   cat("Mulcom loaded\n")
}


