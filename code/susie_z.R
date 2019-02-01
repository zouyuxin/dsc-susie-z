library(susieR)

susie_z_analyze = function(pathR, ss, L, s_init, estimate_residual_variance) {
  R = as.matrix(readRDS(pathR))
  if (!is.list(s_init)){
    fit = susie_z(ss$effect/ss$se, R, L=L, estimate_residual_variance = estimate_residual_variance)
  } else {
    fit = susie_z(ss$effect/ss$se, R, L=L, s_init=s_init, estimate_residual_variance = estimate_residual_variance)
  }
  # get credible sets
  sets = fit$sets
  cs = sets$cs
  purity = sets$purity
  avg_purity = mean(purity[,1])
  cs_index = sets$cs_index
  pip = fit$pip
  niter = fit$niter

  # estimated coefficents
  beta_est_idx = unlist(cs)
  beta_est_val = coef(fit)[-1][beta_est_idx]

  return(list(fit = fit,
              sets = sets,
              cs = cs,
              cs_index = cs_index,
              pip = pip,
              niter = niter,
              beta_est_idx = beta_est_idx, beta_est_val = beta_est_val,
              avg_purity = avg_purity))
}
