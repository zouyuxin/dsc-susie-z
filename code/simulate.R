#' @title sim_gaussian simulates a normal y from given data matrix X
#' @param X an n by p matrix
#' @param pve a scalar percentage variance explained
#' @param effect_num a scalar number of true nonzero effects
#' @param beta.sigma a scalar that used for simulating betas
#' @return train_n a scalar number of trainning samples
#' @return sim_y an n vector simulated gaussian y
#' @return beta_idx a effect_num-length vector that includes indices of effects
#' @return beta_val a effect_num-length vector that includes beta values
#' @return mean_corX mean of correlations of X (lower triangular entries of correlation matrix of X)
sim_gaussian = function(X, pve, effect_num, beta.sigma){
  if (is.na(beta.sigma)){
    beta.sigma = 1
  }
  X = as.matrix(X)
  n = dim(X)[1]
  p = dim(X)[2]

  beta.values = rnorm(effect_num, 0, beta.sigma)
  beta.idx = sample(p, effect_num)
  beta = rep(0,p)
  beta[beta.idx] = beta.values

  if (effect_num==1){
    mean_corX = 1
  } else {
    effectX = X[,beta.idx]
    corX = cor(effectX)
    mean_corX = mean(abs(corX[lower.tri(corX)]))
  }
  if(effect_num==0){
    sim.y = rnorm(n, 0, 1)
  }else{
    y = X %*% beta
    sigma = sqrt(var(y)*(1-pve)/pve)
    epsilon = rnorm(n, mean = 0, sd = sigma)
    sim.y = y + epsilon
  }
  ss <- data.frame(t(apply(X, 2, FUN=function(x){
    fit <- lm(sim.y~x)
    summary(fit)$coefficients[2,1:2]
  })))
  names(ss) <- c("effect", "se")

  return(list(n = n, p = p, sim_y = sim.y,
              beta_idx = beta.idx, beta_val = beta.values, mean_corX = mean_corX,
              ss = ss))
}
