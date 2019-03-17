guess_what_I_do <- function(u){

  out <- dpois(10,lambda=u,log=TRUE)
  return(out)

  }
