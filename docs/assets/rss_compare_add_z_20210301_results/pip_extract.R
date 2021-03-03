input = 'susie_rss_paper_new_lmpve02_query.rds'
output = 'rss_compare_add_z_20210301_pip_extraction/gtex_pip.rds'

library(dplyr)
dat = readRDS(input)
dat_old = readRDS('../rss_compare_add_z_lambda_results/susie_rss_paper_lmpve02_query.rds')
dat$caviar = dat_old$caviar
rm(dat_old)

dat$susie$method = paste0(dat$susie$method_susie, '_ER', dat$susie$method_susie.estimate_residual_variance,
                    '_ld', dat$susie$method_susie.ld_method,
                    '_AZ', dat$susie$method_susie.add_z,
                    '_rcor', dat$susie$method_susie.rcor)

dat$finemap$method = paste0('FINEMAPv1.1',
                          '_ld', dat$finemap$method_finemap.ld_method,
                          '_AZ', dat$finemap$method_finemap.add_z)

dat$finemapv4$method = paste0('FINEMAPv1.4',
                            '_ld', dat$finemapv4$method_finemapv4.ld_method,
                            '_AZ', dat$finemapv4$method_finemapv4.add_z)

dat$caviar$method = paste0('CAVIAR',
                           '_ld', dat$caviar$ld_method,
                           '_AZ', dat$caviar$add_z)

n_signals = unique(dat$susie$lm_pve02.n_signal)

result = list()
for(s in n_signals){
  result[[as.character(s)]] = NULL
  if (s > 3) {
    has_caviar = FALSE
  } else {
    has_caviar = TRUE
  }
  res = list()
  ## susierss
  methods = unique(dat$susie$method)
  for(name in methods){
    dat_sub = dat$susie %>% filter(method == name, lm_pve02.n_signal == s)
    for(i in 1:nrow(dat_sub)){
      true_coef = c(dat_sub[i,]$lm_pve02.meta[[1]]$true_coef != 0)
      pip = c(dat_sub[i,]$score_susie.pip[[1]])
      if (!(name %in% names(res))) {
        res[[name]] = list(pip = pip, truth = true_coef)
      } else {
        res[[name]]$pip = append(res[[name]]$pip, pip)
        res[[name]]$truth = append(res[[name]]$truth, true_coef)
      }
    }
  }
  
  if(has_caviar){
    ## finemap
    methods = unique(dat$finemap$method)
    for(name in methods){
      dat_sub = dat$finemap %>% filter(method == name, lm_pve02.n_signal == s)
      for(i in 1:nrow(dat_sub)){
        true_coef = c(dat_sub[i,]$lm_pve02.meta[[1]]$true_coef != 0)
        pip = c(dat_sub[i,]$score_finemap.pip[[1]])
        if (!(name %in% names(res))) {
          res[[name]] = list(pip = pip, truth = true_coef)
        } else {
          res[[name]]$pip = append(res[[name]]$pip, pip)
          res[[name]]$truth = append(res[[name]]$truth, true_coef)
        }
      }
    }
    ## finemapv1.4
    methods = unique(dat$finemapv4$method)
    for(name in methods){
      dat_sub = dat$finemapv4 %>% filter(method == name, lm_pve02.n_signal == s)
      for(i in 1:nrow(dat_sub)){
        true_coef = c(dat_sub[i,]$lm_pve02.meta[[1]]$true_coef != 0)
        pip = c(dat_sub[i,]$score_finemapv4.pip[[1]])
        if (!(name %in% names(res))) {
          res[[name]] = list(pip = pip, truth = true_coef)
        } else {
          res[[name]]$pip = append(res[[name]]$pip, pip)
          res[[name]]$truth = append(res[[name]]$truth, true_coef)
        }
      }
    }
    ## caviar
    methods = unique(dat$caviar$method)
    for(name in methods){
      dat_sub = dat$caviar %>% filter(method == name, n_signal == s, args == paste0('-g 0.001 -c ', s))
      for(i in 1:nrow(dat_sub)){
        true_coef = c(dat_sub[i,]$meta[[1]]$true_coef != 0)
        pip = c(dat_sub[i,]$pip[[1]])
        if (!(name %in% names(res))) {
          res[[name]] = list(pip = pip, truth = true_coef)
        } else {
          res[[name]]$pip = append(res[[name]]$pip, pip)
          res[[name]]$truth = append(res[[name]]$truth, true_coef)
        }
      }
    }
  }
  result[[as.character(s)]] = do.call(cbind.data.frame, res)
  truth = result[[as.character(s)]][,grep('truth', names(result[[as.character(s)]]))[1]]
  result[[as.character(s)]] = result[[as.character(s)]][,-grep('truth', names(result[[as.character(s)]]))]
  names(result[[as.character(s)]]) = sapply(strsplit(names(result[[as.character(s)]]), '.pip'), function(x) x[1])
  result[[as.character(s)]]$truth = truth
}

saveRDS(result, output)
