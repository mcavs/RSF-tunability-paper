train_rsf_model <- function(task, hyp) {
  
  cv_rsf_10fold <- rsmp("cv", folds = 10)
  cv_rsf_10fold$instantiate(task)

  rsf_model <- lrn("surv.rfsrc", id = "rfsrc",
                   ntree          = hyp$ntree,                     # default: 500
                   mtry           = hyp$mtry,                      # default: sqrt(p)
                   nodesize       = hyp$nodesize,                  # default: 15
                   nodedepth      = hyp$nodedepth,                 # default: NULL - sınırsız
                   splitrule      = as.character(hyp$splitrule),   # default: "logrank"
                   nsplit         = hyp$nsplit)                    # default: 10

  cv_results <- resample(task, rsf_model, cv_rsf_10fold)
  return(list(cindex = cv_results$aggregate(msr("surv.cindex")),
              brier  = cv_results$aggregate(msr("surv.brier")),
              auc    = cv_results$aggregate(msr("surv.uno_auc"))))
  
  
  
}
