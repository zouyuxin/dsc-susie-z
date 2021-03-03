output_dir = 'output/rss_compare_add_z_20210301/'
output = 'rss_compare_add_z_20210301_results/susie_rss_paper_new_lmpve02_query.rds'


library(tibble)
# susie_out = dscrutils::dscquery(output_dir,
#                                     targets = c("small_data.dataset","lm_pve02",
#                                                 "lm_pve02.meta", "lm_pve02.n_signal",
#                                                 "method_susie",
#                                                 "method_susie.ld_method", "method_susie.add_z",
#                                                 "method_susie.rcor",
#                                                 "method_susie.estimate_residual_variance",
#                                                 "method_susie.L", "method_susie.DSC_TIME",
#                                                 "score_susie.total", "score_susie.valid",
#                                                 "score_susie.size", "score_susie.purity", "score_susie.avgr2",
#                                                 "score_susie.top", "score_susie.converged","score_susie.objective",
#                                                 "score_susie.overlap", "score_susie.signal_pip", "score_susie.pip"),
#                                     module.output.files = c("lm_pve02",'method_susie'),
#                                 ignore.missing.files = TRUE)

# caviar_out = dscrutils::dscquery(output_dir,
#                                  targets = c("small_data.dataset", "lm_pve02",
#                                              "lm_pve02.meta", "lm_pve02.n_signal",
#                                              "method_caviar.ld_method", "method_caviar.add_z",
#                                              "method_caviar.args", "method_caviar.DSC_TIME",
#                                              "score_caviar.total", "score_caviar.valid", "score_caviar.size",
#                                              "score_caviar.signal_pip", "score_caviar.pip"),
#                                  module.output.files = "lm_pve02")

finemapv4_out = dscrutils::dscquery(output_dir,
                                  targets = c("small_data.dataset","lm_pve02",
                                              "lm_pve02.meta", "lm_pve02.n_signal",
                                              'method_finemapv4',"method_finemapv4.ld_method",
                                              "method_finemapv4.add_z", "method_finemapv4.DSC_TIME",
                                              "score_finemapv4.total", "score_finemapv4.valid", "score_finemapv4.size",
                                              "score_finemapv4.signal_pip", "score_finemapv4.pip"),
                                  module.output.files =c("lm_pve02",'method_finemapv4'),
                                  ignore.missing.files = TRUE)

# finemap_out = dscrutils::dscquery(output_dir,
#                                   targets = c("small_data.dataset","lm_pve02",
#                                               "lm_pve02.meta", "lm_pve02.n_signal",
#                                               'method_finemap',"method_finemap.ld_method",
#                                               "method_finemap.add_z", "method_finemap.DSC_TIME",
#                                               "score_finemap.total", "score_finemap.valid", "score_finemap.size",
#                                               "score_finemap.signal_pip", "score_finemap.pip"),
#                                   module.output.files =c("lm_pve02",'method_finemap'),
#                                   ignore.missing.files = TRUE)
# rename_cols = function(dat) {
#   for (item in names(dat)) {
#     tmp = strsplit(colnames(dat[[item]]), "[.]")
#     colnames(dat[[item]]) = unlist(lapply(1:length(tmp), function(i) ifelse(length(tmp[[i]])>1, tmp[[i]][2], tmp[[i]][1])))
#   }
#   return(dat)
# }
# remove module names from column names; this is Okay because every column field here are unique
res = readRDS(output)
res$finemapv4 = as_tibble(finemapv4_out)
# res = list(susie=as_tibble(susie_out), finemap=as_tibble(finemap_out), finemapv4=as_tibble(finemapv4_out))
saveRDS(res, output)
