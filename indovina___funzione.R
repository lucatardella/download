guess_what_I_do <- function(u){
  
  out <- prod(dpois(u,lambda=c(5,2,1,3)))
  return(out)
  
}
