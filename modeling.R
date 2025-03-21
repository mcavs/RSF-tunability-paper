train_rsf_model <- function(task, ntree, mtry, nodesize, nodedepth, splitrule, nsplit) {
  
  cv_rsf_10fold <- rsmp("cv", folds = 10)
  cv_rsf_10fold$instantiate(task)

  rsf_model <- lrn("surv.rfsrc", id = "rfsrc",
                   ntree          = ntree,       # default: 500
                   mtry           = mtry,        # default: sqrt(p)
                   nodesize       = nodesize,    # default: 15
                   nodedepth      = 10,          # default: NULL - sınırsız
                   splitrule      = splitrule,   # default: "logrank"
                   nsplit         = 5)           # default: 10

  cv_results <- resample(task, rsf_model, cv_rsf_10fold)

  cv_results$score(msr("surv.cindex"))
  
  cv_summary <- cv_results$aggregate(msr("surv.cindex"))  # aggregate ile birlikte 
                                                          # ortalama C-Index 
                                                          # değerini alıyoruz
  
  cat("C-Index: ", round(cv_summary[["surv.cindex"]], 4), "\n")
  
  return(list(
    model = rsf_model,
    c_index = cv_summary[["surv.cindex"]]
  ))
}
