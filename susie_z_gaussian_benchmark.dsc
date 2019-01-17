# simulate modules

sim_gaussian: simulate.R + \
                R(sim_gaussian_res=sim_gaussian(readRDS(pathX), pve, effect_num, betaSigma))
  pathX: $pathX
  pve: 0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 0.95
  effect_num: 1, 2, 5, 10, 20
  betaSigma: 1
  $n: sim_gaussian_res$n
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
  betaSigma: 0

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
              R(susie_res = susie_z_analyze(pathR, ss, n, L, s_init))
  pathR: $pathR
  ss: $ss
  n: $n
  L: 5, 20
  s_init: 0
  $fit: susie_res$fit
  $sets: susie_res$sets
  $cs: susie_res$cs
  $cs_index: susie_res$cs_index
  $pip: susie_res$pip
  $niter: susie_res$niter
  $beta_est_idx: susie_res$beta_est_idx
  $beta_est_val: susie_res$beta_est_val
  $avg_purity: susie_res$avg_purity

susie_z_large(susie_z):
  L: 201

susie_z_init(susie_z):
  s_init: $s_init
  L: 20

# score modules
score: score.R + R(score_res=compute_scores(cs, beta_idx, pip))
  cs: $cs
  beta_idx: $beta_idx
  pip: $pip
  $hit: score_res$hit
  $signal_num: score_res$signal_num
  $cs_medianSize: score_res$cs_medianSize
  $top_hit: score_res$top_hit
  $dup_bool: score_res$dup_bool

DSC:
  run:
    gaussian_z_null: sim_gaussian_null * susie_z * score
    gaussian_z: sim_gaussian * susie_z * score
    gaussian_z_init: sim_gaussian * init_true * susie_z_init * score

  exec_path: code
  R_libs: MASS, susieR@stephenslab/susieR
  global:
    pathX: "data/susie_X.rds"
    pathR: "data/susie_R.rds"
  output: output/susie_z_gaussian_benchmark

