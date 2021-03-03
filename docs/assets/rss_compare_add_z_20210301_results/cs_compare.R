library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
## Functions
plot_panel = function(dat, quantity, legend = TRUE) {
  p = ggplot(dat, aes_string(x="method", y=quantity[1])) + 
    geom_point(position=position_dodge(.25), size=2.5)
  if (quantity[1] == 'power'){
    p = p + geom_errorbar(aes(ymin=power-power_se, ymax=power+power_se), 
                          width=.2, position=position_dodge(.25))
  }
  if (quantity[1] == 'coverage') {
    p = p + geom_errorbar(aes(ymin=coverage-coverage_se, ymax=coverage+coverage_se), 
                          width=.2, position=position_dodge(.25)) + 
      geom_hline(yintercept = 0.95, colour = 'gray') 
  }
  p = p + labs(x = "method", y = "") + theme_cowplot() + 
    background_grid(major = "x", minor = "none") + 
    ggtitle(quantity[2]) + theme(axis.title.x = element_text(size = 8),
                                 axis.text.x = element_text(angle = 60, size=6,hjust = 1),
                                 plot.title = element_text(size=10))
  if (!legend) p = p + theme(legend.position="none")
  return(p)
}

output = 'rss_compare_add_z_20210301_cs/gtex_cs'

input = 'susie_rss_paper_new_lmpve02_query.rds'
dat = readRDS(input)

dat$susie$method = paste0(dat$susie$method_susie, '_ER', dat$susie$method_susie.estimate_residual_variance,
                          '_ld', dat$susie$method_susie.ld_method,
                          '_AZ', dat$susie$method_susie.add_z,
                          '_rcor', dat$susie$method_susie.rcor)

dat$finemapv4$method = paste0('FINEMAPv1.4',
                              '_ld', dat$finemapv4$method_finemapv4.ld_method,
                              '_AZ', dat$finemapv4$method_finemapv4.add_z)

# for(s in 1:5){
#   ## susierss
#   methods = unique(dat$susie$method)
#   res = list()
#   for(met in methods){
#     print(met)
#     dat_sub = dat$susie %>% filter(method == met, lm_pve02.n_signal == s)
#     converged = unlist(dat_sub$score_susie.converged)
#     total = unlist(dat_sub$score_susie.total) * converged
#     valid = unlist(dat_sub$score_susie.valid) * converged
#     sizes = unlist(dat_sub$score_susie.size[converged])
#     purity = unlist(dat_sub$score_susie.purity[converged])
#     expected = unlist(dat_sub$lm_pve02.n_signal) * converged
#     
#     res[[met]] = list(total = sum(total), valid = sum(valid),
#                       size = median(sizes, na.rm=T), purity = median(purity, na.rm=T),
#                       expected = sum(expected), nonconverged = sum(!converged))
#     res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#     res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#     res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#     res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#     res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
#   }
#   saveRDS(res, paste0(output, '_susierss_s', s,'.rds'))
# }
# 
# for(s in 1:3){
#   ## fm
#   methods = unique(dat$finemapv4$method)
#   res = list()
#   for(met in methods){
#     print(met)
#     dat_sub = dat$finemapv4 %>% filter(method == met, lm_pve02.n_signal == s)
#     total = unlist(dat_sub$score_finemapv4.total)
#     valid = unlist(dat_sub$score_finemapv4.valid)
#     sizes = unlist(dat_sub$score_finemapv4.size)
#     expected = unlist(dat_sub$lm_pve02.n_signal)*2
#     
#     res[[met]] = list(total = sum(total), valid = sum(valid),
#                       size = median(sizes, na.rm=T), purity = NA,
#                       expected = sum(expected), nonconverged = NA)
#     res[[met]]$power = res[[met]]$valid/res[[met]]$expected
#     res[[met]]$coverage = res[[met]]$valid/res[[met]]$total
#     res[[met]]$power_se = sqrt(res[[met]]$power * (1-res[[met]]$power) / res[[met]]$expected)
#     res[[met]]$power_se[is.nan(res[[met]]$power_se)] = 0
#     res[[met]]$coverage_se = sqrt(res[[met]]$coverage * (1-res[[met]]$coverage) / res[[met]]$total)
#   }
#   saveRDS(res, paste0(output, '_fmv4_s', s, '.rds'))
# }


rename = list("susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE" = 'susie',
              "susie_rss_ERTRUE_ldin_sample_AZFALSE_rcorTRUE" = "susie_rss",
              "susie_rss_ERTRUE_ldout_sample_AZFALSE_rcorTRUE" = "susie_rss_ldref",
              "susie_rss_add_z_ERTRUE_ldout_sample_AZTRUE_rcorTRUE" = "susie_rss_ldref_AZ_cor",
              "susie_rss_add_z_ERTRUE_ldout_sample_AZTRUE_rcorFALSE" = "susie_rss_ldref_AZ_cov",
              "susie_rss_lambda_ERTRUE_ldin_sample_AZFALSE_rcorTRUE" = "susie_rss_lambda",
              "susie_rss_lambda_ERTRUE_ldout_sample_AZFALSE_rcorTRUE" = "susie_rss_lambda_ldref",
              "susie_rss_lambda_add_z_ERTRUE_ldout_sample_AZTRUE_rcorTRUE" = "susie_rss_lambda_ldref_AZ_cor",
              "susie_rss_lambda_add_z_ERTRUE_ldout_sample_AZTRUE_rcorFALSE" = "susie_rss_lambda_ldref_AZ_cov",
              "FINEMAPv1.1_ldin_sample_AZFALSE" = 'FINEMAPv1.1',
              "FINEMAPv1.1_ldout_sample_AZFALSE" = 'FINEMAPv1.1_ldref',
              "FINEMAPv1.1_ldout_sample_AZTRUE" = 'FINEMAPv1.1_ldref_AZ_cor',
              "FINEMAPv1.4_ldin_sample_AZFALSE" = 'FINEMAPv1.4',
              "FINEMAPv1.4_ldout_sample_AZFALSE" = 'FINEMAPv1.4_ldref',
              "FINEMAPv1.4_ldout_sample_AZTRUE" = 'FINEMAPv1.4_ldref_AZ_cor',
              'CAVIAR_ldin_sample_AZFALSE' = 'CAVIAR',
              'CAVIAR_ldout_sample_AZFALSE' = 'CAVIAR_ldref',
              'CAVIAR_ldout_sample_AZTRUE' = 'CAVIAR_ldref_AZ_cor')

for(s in 1:3){
  res_susierss = readRDS(paste0(output, '_susierss_s',s,'.rds'))
  res_fm = readRDS(paste0(output, '_fmv4_s', s,'.rds'))
  res = c(res_susierss, res_fm)
  rates = matrix(unlist(res), length(res), byrow = T)
  rownames(rates) = names(res)
  colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'expected', 'nonconverge',
                      'power', 'coverage', 'power_se', 'coverage_se')
  rates = as.data.frame(rates)
  rates$method = rownames(rates)
  
  rates_ERTRUE = rates[-grep('ERFALSE', rates$method),]
  rates_ERTRUE$method = sapply(rownames(rates_ERTRUE), function(x) rename[[x]])
  
  rates_ERTRUE$method = factor(rates_ERTRUE$method, levels = c('susie', 'susie_rss', 'susie_rss_ldref',
                                                               'susie_rss_ldref_AZ_cor', 'susie_rss_ldref_AZ_cov',
                                                               'susie_rss_lambda', 'susie_rss_lambda_ldref',
                                                               'susie_rss_lambda_ldref_AZ_cor','susie_rss_lambda_ldref_AZ_cov',
                                                               'FINEMAPv1.4', 'FINEMAPv1.4_ldref', 'FINEMAPv1.4_ldref_AZ_cor'))
  p1 = plot_panel(rates_ERTRUE, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_ERTRUE, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_ERTRUE, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_ERTRUE, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_s',s,'_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_s',s,'_plots.pdf'), " ", paste0(output, '_s',s,'_plots.png')))
}

for(s in 4:5){
  res_susierss = readRDS(paste0(output, '_susierss_s',s,'.rds'))
  res = c(res_susierss)
  rates = matrix(unlist(res), length(res), byrow = T)
  rownames(rates) = names(res)
  colnames(rates) = c('discoveries', 'valid', 'size', 'purity', 'expected', 'nonconverge',
                      'power', 'coverage', 'power_se', 'coverage_se')
  rates = as.data.frame(rates)
  rates$method = rownames(rates)
  
  rates_ERTRUE = rates[-grep('ERFALSE', rates$method),]
  rates_ERTRUE$method = sapply(rownames(rates_ERTRUE), function(x) rename[[x]])
  
  rates_ERTRUE$method = factor(rates_ERTRUE$method, levels = c('susie', 'susie_rss', 'susie_rss_ldref',
                                                               'susie_rss_ldref_AZ_cor', 'susie_rss_ldref_AZ_cov',
                                                               'susie_rss_lambda', 'susie_rss_lambda_ldref',
                                                               'susie_rss_lambda_ldref_AZ_cor','susie_rss_lambda_ldref_AZ_cov'))
  p1 = plot_panel(rates_ERTRUE, c('coverage', 'coverage'), legend=F)
  p2 = plot_panel(rates_ERTRUE, c('power', 'power'), legend=F)
  p3 = plot_panel(rates_ERTRUE, c('size', 'median number of variables'), legend=F)
  p4 = plot_panel(rates_ERTRUE, c('purity', 'median of purity'), legend=F)
  pdf(paste0(output, '_s',s,'_plots.pdf'), width=12, height=3)
  grid.arrange(p1,p2,p3,p4, ncol=4, widths=c(3,3,3,3))
  dev.off()
  system(paste0("convert -flatten -density 120 ", paste0(output, '_s',s,'_plots.pdf'), " ", paste0(output, '_s',s,'_plots.png')))
}


