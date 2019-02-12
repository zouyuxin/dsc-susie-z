#!/usr/bin/env dsc

# simulate modules

sim_gaussian: simulate.R + \
                R(sim_gaussian_res=sim_gaussian(readRDS(pathX), pve, effect_num, equal_eff))
  pathX: $pathX
  pve: 0.01, 0.2, 0.6, 0.8
  effect_num: 1, 2, 5, 10, 20
  equal_eff: rep(1/effect_num, effect_num), c(rep(0.2/(effect_num-1), effect_num-1), 0.8)
  $p: sim_gaussian_res$p
  $sigma: sim_gaussian_res$sigma
  $sim_y: sim_gaussian_res$sim_y
  $beta_idx: sim_gaussian_res$beta_idx
  $beta_val: sim_gaussian_res$beta_val
  $mean_corX: sim_gaussian_res$mean_corX
  $ss: sim_gaussian_res$ss

sim_gaussian_null(sim_gaussian):
  pve: 0
  effect_num: 0
  equal_eff: TRUE

sim_gaussian_large(sim_gaussian):
  effect_num: 200

# initialize modulas
init_true: initialize.R + \
                        R(s_init=init_susie(beta_idx, beta_val, p))
  beta_idx: $beta_idx
  beta_val: $beta_val
  p: $p
  $s_init: s_init

# susie_z modulas
susie_z: susie_z.R + \
              R(susie_res = susie_z_analyze(pathR, ss, L, s_init, estimate_residual_variance))
  pathR: $pathR
  ss: $ss
  L: 5, 20
  s_init: 0
  estimate_residual_variance: TRUE
  $fit: susie_res

susie_z_large(susie_z):
  L: 201

susie_z_init(susie_z):
  s_init: $s_init
  L: 20

# score modules
score: score.R + R(score_res=compute_scores(sets, pip, beta_idx))
  sets: $(fit)$sets
  pip: $(fit)$pip
  beta_idx: $beta_idx
  $total: score_res$total
  $valid: score_res$valid
  $size: median(score_res$size)
  $purity: median(score_res$purity)
  $top: score_res$top

DSC:
  run:
    gaussian_z_null: sim_gaussian_null * susie_z * score
    gaussian_z: sim_gaussian * susie_z * score
    gaussian_z_init: sim_gaussian * init_true * susie_z_init * score

  exec_path: code
  R_libs: MASS, susieR@stephenslab/susieR
  global:
    pathX: "data/X_random.rds"
    pathR: "data/R_random.rds"
  output: output/susie_z_estimate_residual_variance

