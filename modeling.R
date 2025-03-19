train_rsf_model <- function(task, mtry, ntree, nodesize, splitrule) {
  
  cv_rsf_10fold <- rsmp("cv", folds = 10)
  cv_rsf_10fold$instantiate(task)

  rsf_model <- lrn("surv.rfsrc", id = "rfsrc",
                   ntree = ntree,
                   mtry = mtry,
                   nodesize = nodesize,
                   splitrule = splitrule)

  cv_results <- resample(task, rsf_model, cv_rsf_10fold)

  cv_summary <- cv_results$aggregate(msr("surv.cindex"))  # aggregate ile birlikte 
                                                          # ortalama C-Index 
                                                          # değerini alıyoruz
  
  cat("C-Index: ", round(cv_summary[["surv.cindex"]], 4), "\n")
  
  return(list(
    model = rsf_model,
    c_index = cv_summary[["surv.cindex"]]
  ))
}
