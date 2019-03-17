guess_what_I_do <- function(u){

  out <- dpois(u,lambda=10,log=TRUE)
  return(out)

  }
