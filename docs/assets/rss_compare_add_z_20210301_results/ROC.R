library(dplyr)
library(scam)
## Functions
roc_data = function(d1, cutoff = c(pip_cutoff, 1), connect_org = FALSE) {
  grid = 500
  ttv = seq(1:grid)/grid
  ttv = ttv[which(ttv>=cutoff[1] & ttv<=cutoff[2])]
  # see SuSiE-Manuscript issue 2
  d1 = d1[order(d1[,1]), ]
  end = tail(d1[which(d1[,2] == 0),][,1],1)
  ttv = c(ttv[-length(ttv)], min(ttv[length(ttv)], end))
  # end of issue 2
  rst1 = t(sapply(ttv, function(x) c(sum(d1[,2][d1[,1]>=x]), length(d1[,2][d1[,1]>=x]))))
  rst1 = cbind(rst1, sum(d1[,2]), sum(1-d1[,2]))
  # colnames(rst1) = c('true_positive', 'total_positive', 'total_signal','total_null')
  if (connect_org) {
    # connect to origin
    last_row = tail(rst1, 1)
    rst1 = rbind(rst1, c(last_row[1], last_row[2]-1, last_row[3]), c(0.001,0.001,last_row[3]))
  }
  rst1 = as.data.frame(rst1)
  colnames(rst1) = c('true_positive', 'total_positive', 'total_signal', 'total_null')
  if (connect_org) {
    rst2 = as.data.frame(cbind(rst1$true_positive / rst1$total_positive, rst1$true_positive / rst1$total_signal,  c(ttv, ttv[length(ttv)], 1)))
  } else {
    rst2 = as.data.frame(cbind(rst1$true_positive / rst1$total_positive, rst1$true_positive / rst1$total_signal,  ttv, 
                               (rst1$total_positive - rst1$true_positive)/rst1$total_null))
  }
  colnames(rst2) = c('Precision', 'Recall', 'Threshold', 'FPR')
  return(list(counts = rst1, rates = rst2))
}


create_chunks = function(item, n) {
  splitted = suppressWarnings(split(item, 1:n))
  return(c(splitted[[1]], splitted[[length(splitted)]][length(splitted[[length(splitted)]])]))
}
make_smooth = function(x,y,subset=chunks, sm = smooth) {
  if (sm) {
    if (subset < length(x) && subset > 0) {
      x = create_chunks(x, subset)
      y = create_chunks(y, subset)
    }
    dat = data.frame(cbind(x,y))
    colnames(dat) = c('x','y')
    y=predict(scam(y ~ s(x, bs = "mpi"), data = dat))
  }
  return(list(x=x,y=y))
}
add_text = function(thresholds, x, y, threshold, color, delta = 0.015, y.delta=0) {
  idx = which(thresholds == threshold)
  # text(x[idx] - delta, y[idx] +y.delta, labels = threshold, col = color)
  points(x[idx],y[idx], col = color, cex = 1.1, lwd=2.5)
}

pip_cutoff = 0.3

chunks = 0
smooth = FALSE
colors = c('#A60628', '#7A68A6', '#348ABD', '#467821', '#FF00FF', '#E2A233',
           '#00FFFF', '#A9A9A9', '#ADFF2F', '#188487',  '#FF0000', '#000000',
           '#FFD700', '#00FF00', '#9400D3', '#7FFFD4', '#A52A2A', '#000080')
input = paste0('rss_compare_add_z_20210301_pip_extraction/gtex_pip.rds')
output = paste0('rss_compare_add_z_20210301_roc/gtex_roc')

dat = readRDS(input)
# print("Computing ROC data ...")
# 
# tb = list()
# methods = names(dat$`1`)
# for (method in methods) {
#   print(method)
#   tmp = do.call(rbind, lapply(1:3, function(i) cbind(dat[[i]][[method]], dat[[i]]$truth)))
#   tb[[method]] = roc_data(tmp)
# }
# saveRDS(tb, paste0(output, '_3.rds'))
# 
# tb = list()
# methods = names(dat$`4`)
# for (method in methods) {
#   print(method)
#   tmp = do.call(rbind, lapply(1:5, function(i) cbind(dat[[i]][[method]], dat[[i]]$truth)))
#   tb[[method]] = roc_data(tmp)
# }
# saveRDS(tb, paste0(output, '_5.rds'))

rename_resid = list("susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE" = 'susie_ERTRUE',
                    "susie_suff_ERFALSE_ldin_sample_AZFALSE_rcorTRUE" = 'susie_ERFALSE',
                    "susie_rss_ERTRUE_ldin_sample_AZFALSE_rcorTRUE" = "susie_rss_ERTRUE",
                    "susie_rss_ERFALSE_ldin_sample_AZFALSE_rcorTRUE" = "susie_rss_ERFALSE",
                    "susie_rss_lambda_ERTRUE_ldin_sample_AZFALSE_rcorTRUE" = "susie_rss_lambda_ERTRUE",
                    "susie_rss_lambda_ERFALSE_ldin_sample_AZFALSE_rcorTRUE" = "susie_rss_lambda_ERFALSE")

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

## plots
d1 = readRDS(paste0(output, '_3.rds'))
d2 = readRDS(paste0(output, '_5.rds'))
xlim = 0.3
ylim = 0.3
main = "FDR vs Power"
ylab = "power"
xlab = "FDR"
# residuals

pdf(paste0(output,"_5_susie_residuals.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d2
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 'susie_suff_ERFALSE_ldin_sample_AZFALSE_rcorTRUE')) {
  yy = make_smooth((1 - tb[[method]]$rates[,1]), tb[[method]]$rates[,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename_resid[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_5_susie_residuals.pdf', " ",
              output, '_5_susie_residuals.png'))

pdf(paste0(output,"_5_susierss_residuals.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d2
labels = vector()
for (method in c('susie_rss_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 'susie_rss_ERFALSE_ldin_sample_AZFALSE_rcorTRUE')) {
  yy = make_smooth((1 - tb[[method]]$rates[,1]), tb[[method]]$rates[,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename_resid[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_5_susierss_residuals.pdf', " ",
              output, '_5_susierss_residuals.png'))

pdf(paste0(output,"_5_susiersslambda_residuals.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d2
labels = vector()
for (method in c('susie_rss_lambda_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 'susie_rss_lambda_ERFALSE_ldin_sample_AZFALSE_rcorTRUE')) {
  yy = make_smooth((1 - tb[[method]]$rates[,1]), tb[[method]]$rates[,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename_resid[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_5_susiersslambda_residuals.pdf', " ",
              output, '_5_susiersslambda_residuals.png'))

## In sample
pdf(paste0(output,"_3_ldin.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d1
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 'susie_rss_ERTRUE_ldin_sample_AZFALSE_rcorTRUE',
                 'susie_rss_lambda_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 'FINEMAPv1.1_ldin_sample_AZFALSE',
                 'FINEMAPv1.4_ldin_sample_AZFALSE', 'CAVIAR_ldin_sample_AZFALSE')) {
  yy = make_smooth((1 - tb[[method]]$rates[,1]), tb[[method]]$rates[,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_3_ldin.pdf', " ",
              output, '_3_ldin.png'))

pdf(paste0(output,"_5_ldin.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d2
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 'susie_rss_ERTRUE_ldin_sample_AZFALSE_rcorTRUE',
                 'susie_rss_lambda_ERTRUE_ldin_sample_AZFALSE_rcorTRUE')) {
  yy = make_smooth((1 - tb[[method]]$rates[,1]), tb[[method]]$rates[,2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[,3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_5_ldin.pdf', " ",
              output, '_5_ldin.png'))

## susierss
pdf(paste0(output,"_5_susierss.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d2
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 
                 'susie_rss_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 'susie_rss_ERTRUE_ldout_sample_AZFALSE_rcorTRUE',
                 'susie_rss_add_z_ERTRUE_ldout_sample_AZTRUE_rcorTRUE', 'susie_rss_add_z_ERTRUE_ldout_sample_AZTRUE_rcorFALSE')) {
  yy = make_smooth((1 - tb[[method]]$rates[-nrow(tb[[method]]$rates),1]), tb[[method]]$rates[-nrow(tb[[method]]$rates),2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[-nrow(tb[[method]]$rates),3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_5_susierss.pdf', " ",
              output, '_5_susierss.png'))

pdf(paste0(output,"_5_susiersslambda.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d2
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 
                 'susie_rss_lambda_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 'susie_rss_lambda_ERTRUE_ldout_sample_AZFALSE_rcorTRUE',
                 'susie_rss_lambda_add_z_ERTRUE_ldout_sample_AZTRUE_rcorTRUE', 'susie_rss_lambda_add_z_ERTRUE_ldout_sample_AZTRUE_rcorFALSE')) {
  yy = make_smooth((1 - tb[[method]]$rates[-nrow(tb[[method]]$rates),1]), tb[[method]]$rates[-nrow(tb[[method]]$rates),2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[-nrow(tb[[method]]$rates),3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_5_susiersslambda.pdf', " ",
              output, '_5_susiersslambda.png'))

## finemap
pdf(paste0(output,"_3_finemapv1.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d1
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 
                 'FINEMAPv1.1_ldin_sample_AZFALSE', 'FINEMAPv1.1_ldout_sample_AZFALSE',
                 'FINEMAPv1.1_ldout_sample_AZTRUE')) {
  yy = make_smooth((1 - tb[[method]]$rates[-nrow(tb[[method]]$rates),1]), tb[[method]]$rates[-nrow(tb[[method]]$rates),2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[-nrow(tb[[method]]$rates),3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_3_finemapv1.pdf', " ",
              output, '_3_finemapv1.png'))

## finemapv4
pdf(paste0(output,"_3_finemapv4.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d1
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 
                 'FINEMAPv1.4_ldin_sample_AZFALSE', 'FINEMAPv1.4_ldout_sample_AZFALSE',
                 'FINEMAPv1.4_ldout_sample_AZTRUE')) {
  yy = make_smooth((1 - tb[[method]]$rates[-nrow(tb[[method]]$rates),1]), tb[[method]]$rates[-nrow(tb[[method]]$rates),2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[-nrow(tb[[method]]$rates),3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_3_finemapv4.pdf', " ",
              output, '_3_finemapv4.png'))

## caviar
pdf(paste0(output,"_3_caviar.pdf"), width=5, height=5, pointsize=15)
i = 1
tb = d1
labels = vector()
for (method in c('susie_suff_ERTRUE_ldin_sample_AZFALSE_rcorTRUE', 
                 'CAVIAR_ldin_sample_AZFALSE', 'CAVIAR_ldout_sample_AZFALSE',
                 'CAVIAR_ldout_sample_AZTRUE')) {
  yy = make_smooth((1 - tb[[method]]$rates[-nrow(tb[[method]]$rates),1]), tb[[method]]$rates[-nrow(tb[[method]]$rates),2])
  if (i == 1) {
    plot(yy$x, yy$y, t="l", col=colors[i], ylab = ylab, xlab = xlab, main = main, bty='l',
         lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  } else {
    lines(yy$x, yy$y, col=colors[i], lwd = 2, xlim = c(0,xlim), ylim = c(0,ylim))
  }
  add_text(tb[[method]]$rates[-nrow(tb[[method]]$rates),3], yy$x, yy$y, 0.95, colors[i])
  labels[i] = rename[[method]]
  i = i + 1
}
legend("topleft", legend=labels, col=colors[1:i], lty=1, cex=0.8)
dev.off()
system(paste0("convert -flatten -density 120 ", output, '_3_caviar.pdf', " ",
              output, '_3_caviar.png'))
