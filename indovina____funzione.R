guess_what_I_do <- function(u){

  out <- sum(dpois(c(5,2,1,3),lambda=u,log=TRUE))
  return(out)

  }
