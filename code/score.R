#' @title Compute and save five variables for power, fdr, median confidence sets size, whether hit the top pip, whether fit has duplicates
#'        we assume that if a susie confidence set contains a true signal, then that set is 'hit'
#' @param fit susie fit object
#' @param beta_idx a vector of index for true signals
#' @return valid a scalar the number of right cs sets
#' @return total a scalar the number of confidence sets
#' @return size a vector with size of each cs
#' @return purity a vector with the purity of each cs
#' @return top_hit a scalar counts how many confidence sets contain a true signal and the true signal has the highest pip
compute_scores = function(fit, beta_idx){
  sets = fit$sets
  pip = fit$pip
  cs = sets$cs
  if (is.null(cs)){
    size = 0
    total = 0
    purity = 0
  } else {
    size = sapply(cs, length)
    total = length(cs)
    purity = as.vector(sets$purity[,1])
  }
  valid = 0
  top_hit = 0
  if(total > 0){
    for( i in 1:total){
      if(any(cs[[i]] %in% beta_idx)) valid = valid + 1
      set.idx = cs[[i]]
      hightest.idx = which.max(pip[set.idx])
      if(set.idx[hightest.idx] %in% beta_idx) top_hit = top_hit + 1
    }
  }
  return(list(total=total, valid=valid, size=size, purity=purity, top=top_hit, converge = fit$converged))
}

#' @title Check if produced confidence sets are duplicated
#' @param cs a list a susie confidence sets from susie fit
#' @return a boolean 1 if duplicated, 0 otherwise
check_duplicate = function(cs){
  cs.length = length(cs)
  if (cs.length == 0){
    return(0)
  }else{
    cs.vec = unlist(cs)
    if (sum(duplicated(cs.vec)) > 0){
      return(1)
    }else{
      return(0)
    }
  }
}

