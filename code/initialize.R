library(susieR)
init_susie = function(beta_idx, beta_val, p){
  if(length(beta_idx) == 0){
    return(0)
  }else{
    return(susie_init_coef(beta_idx, beta_val, p))
  }
}
