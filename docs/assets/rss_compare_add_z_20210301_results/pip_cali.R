library(ggplot2)
library(cowplot)
get_cali = function(alist, col) {
  res = alist[[1]][[col]]
  if(length(alist) > 1){
    for (i in 2:length(alist)) {
      if (!is.null(alist[[i]][[col]])) res = res + alist[[i]][[col]]
    }
  }
  res[,c(1,2)] = res[,c(1,2)] / res[,3]
  return(res[-1,])
}
dot_plot = function(dataframe) {
  ggplot(dataframe, aes(x=mean_pip, y=observed_freq)) + 
    geom_errorbar(aes(ymin=observed_freq-se, ymax=observed_freq+se), colour="gray", size = 0.2, width=.01) +
    geom_point(size=1.5, shape=21, fill="#002b36") + # 21 is filled circle
    xlab("Mean PIP") +
    ylab("Observed frequency") +
    coord_cartesian(ylim=c(0,1), xlim=c(0,1)) +
    geom_abline(slope=1,intercept=0,colour='red', size=0.2) +
    ggtitle(rename[[name]]) +
    expand_limits(y=0) +                        # Expand y range
    theme_cowplot() + theme(plot.title = element_text(size = 8))
}

# parameters
bin_size = 10

input = 'rss_compare_add_z_20210301_pip_extraction/gtex_pip.rds'
output = 'rss_compare_add_z_20210301_pip_calibration/gtex_pip_calib'

dat = readRDS(input)

bins = cbind(seq(1:bin_size)/bin_size-1/bin_size, seq(1:bin_size)/bin_size)
pip_cali = list()
for (s in 1:5) {
  res = dat[[as.character(s)]]
  pip_cali[[as.character(s)]] = list()
  for (name in rev(colnames(res))[-1]) {
    for (i in 1:nrow(bins)) {
      tmp = res[which(res[[name]] > bins[i,1] & res[[name]] < bins[i,2]),]
      if (is.null(pip_cali[[as.character(s)]][[name]])) pip_cali[[as.character(s)]][[name]] = c(sum(tmp[[name]]), sum(tmp$truth), length(tmp$truth))
      else pip_cali[[as.character(s)]][[name]] = rbind(pip_cali[[as.character(s)]][[name]], c(sum(tmp[[name]]), sum(tmp$truth), length(tmp$truth)))
    }
    pip_cali[[as.character(s)]][[name]][which(is.na(pip_cali[[as.character(s)]][[name]]))] = 0
  }
}

methods = names(pip_cali[[1]])
res = lapply(methods, function(name) get_cali(pip_cali, name))
names(res) = methods

saveRDS(res, paste0(output, '.rds'))

pip_cali = readRDS(paste0(output, '.rds'))

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

## susierss
methods = c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE',
            'susie_rss_ERTRUE_ldin_sample_AZFALSE_rcorTRUE',
            'susie_rss_ERTRUE_ldout_sample_AZFALSE_rcorTRUE',
            'susie_rss_add_z_ERTRUE_ldout_sample_AZTRUE_rcorTRUE',
            'susie_rss_add_z_ERTRUE_ldout_sample_AZTRUE_rcorFALSE')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_susierss_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_susierss_' , idx, '.pdf', " ",output, '_susierss_' , idx, '.png'))
}

files = paste0(output, '_susierss_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_susierss.png'))
system(cmd)

## susiersslambda
methods = c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE',
            'susie_rss_lambda_ERTRUE_ldin_sample_AZFALSE_rcorTRUE',
            'susie_rss_lambda_ERTRUE_ldout_sample_AZFALSE_rcorTRUE',
            'susie_rss_lambda_add_z_ERTRUE_ldout_sample_AZTRUE_rcorTRUE',
            'susie_rss_lambda_add_z_ERTRUE_ldout_sample_AZTRUE_rcorFALSE')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_susiersslambda_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_susiersslambda_' , idx, '.pdf', " ",output, '_susiersslambda_' , idx, '.png'))
}

files = paste0(output, '_susiersslambda_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_susiersslambda.png'))
system(cmd)

## caviar
methods = c('CAVIAR_ldin_sample_AZFALSE',
            'CAVIAR_ldout_sample_AZFALSE',
            'CAVIAR_ldout_sample_AZTRUE')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_caviar_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_caviar_' , idx, '.pdf', " ",output, '_caviar_' , idx, '.png'))
}

files = paste0(output, '_caviar_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_caviar.png'))
system(cmd)

## finemap
methods = c('FINEMAPv1.1_ldin_sample_AZFALSE',
            'FINEMAPv1.1_ldout_sample_AZFALSE',
            'FINEMAPv1.1_ldout_sample_AZTRUE')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_fmv1_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_fmv1_' , idx, '.pdf', " ",output, '_fmv1_' , idx, '.png'))
}

files = paste0(output, '_fmv1_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_fmv1.png'))
system(cmd)

## finemapv1.4
methods = c('FINEMAPv1.4_ldin_sample_AZFALSE',
            'FINEMAPv1.4_ldout_sample_AZFALSE',
            'FINEMAPv1.4_ldout_sample_AZTRUE')
idx = 0
for (name in methods) {
  idx = idx + 1
  pip_cali[[name]][,3] = sqrt(pip_cali[[name]][,2] * (1 - pip_cali[[name]][,2]) / pip_cali[[name]][,3]) * 2
  pip_cali[[name]] = as.data.frame(pip_cali[[name]])
  colnames(pip_cali[[name]]) = c("mean_pip", "observed_freq", "se")
  pdf(paste0(output, '_fmv4_' , idx, '.pdf'), width=3, height=3, pointsize=16)
  print(dot_plot(pip_cali[[name]]))
  dev.off()
  system(paste0("convert -flatten -density 120 ", output, '_fmv4_' , idx, '.pdf', " ",output, '_fmv4_' , idx, '.png'))
}

files = paste0(output, '_fmv4_', 1:idx, '.png')
cmd = paste('convert +append', paste(files, collapse=" "), paste0(output, '_fmv4.png'))
system(cmd)

