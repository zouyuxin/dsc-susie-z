library(susieR)

susie_z_analyze = function(pathR, ss, L, s_init, estimate_residual_variance) {
  R = as.matrix(readRDS(pathR))
  if (!is.list(s_init)){
    fit = susie_z(ss$effect/ss$se, R, L=L, estimate_residual_variance = estimate_residual_variance, max_iter = 1000)
  } else {
    fit = susie_z(ss$effect/ss$se, R, L=L, s_init=s_init, estimate_residual_variance = estimate_residual_variance, max_iter = 1000)
  }
  return(fit)
}
