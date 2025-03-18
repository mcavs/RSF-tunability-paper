measure_performance <- function(models, task, split){
  preds   <- NULL
  c_index <- NULL
  brier   <- NULL
  auc     <- NULL
  suppressWarnings(for(i in 1:length(models)){
    preds[[i]] <- models[[i]]$predict(task, row_ids = split$test)
    c_index[i] <- as.numeric(preds[[i]]$score(msr("surv.cindex"),
                                              task = task))
    brier[i]   <- as.numeric(preds[[i]]$score(msr("surv.brier", proper = TRUE),
                                              task = task,
                                              train_set = split$train))
    auc[i]     <- as.numeric(preds[[i]]$score(msr("surv.uno_auc", integrated = TRUE),
                                              task = task,
                                              train_set = split$train))
  })
  
  names(c_index) <- sapply(models, function(x) x$id)
  names(brier)   <- sapply(models, function(x) x$id)
  names(auc)     <- sapply(models, function(x) x$id)
  return(list(c_index = c_index,
              brier   = brier,
              auc     = auc,
              preds   = preds))
}