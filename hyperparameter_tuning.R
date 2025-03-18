optimize_rsf_10fold <- function(ntree_values = c(100, 200, 500, 1000, 2000),
                                mtry_values = c(5, 10, 15, 20),
                                nodesize_values = c(5, 10, 20, 50),
                                splitrule_values = c("logrank", "logrankscore")) {
  
  results <- data.frame(ntree = integer(), mtry = integer(), nodesize = integer(),
                        splitrule = character(), c_index = numeric())
  
  cv_rsf_10fold <- rsmp("cv", folds = 10)  # 10 Katlı CV tanımlıyoruz
  
  for (ntree in ntree_values) {
    for (mtry in mtry_values) {
      for (nodesize in nodesize_values) {
        for (splitrule in splitrule_values) {
          cat("Testing ntree =", ntree, "mtry =", mtry, "nodesize =", nodesize, "splitrule =", splitrule, "\n")
          
          
          model <- lrn("surv.rfsrc", id = "rfsrc", ntree = ntree, mtry = mtry, 
                       nodesize = nodesize, splitrule = splitrule)
          
          
          cv_rsf_10fold$instantiate(task)
          cv_results <- resample(task, model, cv_rsf_10fold)
          
          
          cv_summary <- cv_results$aggregate(msrs(c("surv.cindex")))
          
          # Sonuçları kaydet
          results <- rbind(results, data.frame(
            ntree = ntree,
            mtry = mtry,
            nodesize = nodesize,
            splitrule = splitrule,
            c_index = cv_summary[["surv.cindex"]]
          ))
        }
      }
    }
  }
  
  print(" 10-FOLD CV SONUÇLARI ")
  print(results)
  
  
  best_model <- results[which.max(results$c_index), ]
  print(" EN İYİ HİPERPARAMETRE KOMBİNASYONU ")
  print(best_model)
  
  return(best_model)
}

best_hyperparams_10fold <- optimize_rsf_10fold()







#optimize_rsf_10fold <- function(ntree_values = c(100, 200, 500, 1000, 2000),
#                                mtry_values = c(5, 10, 15, 20),
#                                nodesize_values = c(5, 10, 20, 50),
#                                splitrule_values = c("logrank", "logrankscore"),
#                                sampsize_values = c(0.5, 0.7, 1.0),
#                                nsplit_values = c(1, 2, 5, 10)) {
# 
# results <- data.frame(ntree = integer(), mtry = integer(), nodesize = integer(),
#                       splitrule = character(), sampsize = numeric(), nsplit = integer(),
#                       c_index = numeric())
# 
# cv_rsf_10fold <- rsmp("cv", folds = 10)  # 10 Katlı CV tanımlıyoruz
# 
# for (ntree in ntree_values) {
#   for (mtry in mtry_values) {
#      for (nodesize in nodesize_values) {
#        for (splitrule in splitrule_values) {
#          for (sampsize in sampsize_values) {
#            for (nsplit in nsplit_values) {
#              cat("Testing ntree =", ntree, "mtry =", mtry, "nodesize =", nodesize,
#                 "splitrule =", splitrule, "sampsize =", sampsize, "nsplit =", nsplit, "\n")
#              
#              model <- lrn("surv.rfsrc", id = "rfsrc", ntree = ntree, mtry = mtry, 
#                           nodesize = nodesize, splitrule = splitrule, 
#                           sampsize = sampsize, nsplit = nsplit)
#              
#              cv_rsf_10fold$instantiate(task)
#              cv_results <- resample(task, model, cv_rsf_10fold)
#             
#              cv_summary <- cv_results$aggregate(msrs(c("surv.cindex")))
#              
#              # Sonuçları kaydet
#              results <- rbind(results, data.frame(
#                ntree = ntree,
#                mtry = mtry,
#                nodesize = nodesize,
#                splitrule = splitrule,
#                sampsize = sampsize,
#                nsplit = nsplit,
#                c_index = cv_summary[["surv.cindex"]]
#              ))
#            }
#          }
#        }
#      }
#    }
#  }
#  
#  print(" 10-FOLD CV SONUÇLARI ")
#  print(results)
#  
# best_model <- results[which.max(results$c_index), ]
#  print(" EN İYİ HİPERPARAMETRE KOMBİNASYONU ")
#  print(best_model)
#  
#  return(best_model)
#}
#
#best_hyperparams_10fold <- optimize_rsf_10fold()
