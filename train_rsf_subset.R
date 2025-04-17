train_rsf_subset <- function(task, start_id, end_id, seed = 123) {

  set.seed(seed) 
  
  hyp <- expand.grid(
    ntree     = seq(100, 2000, 200),
    mtry      = seq(1:10),
    nodesize  = seq(5, 100, 10),
    nodedepth = seq(5, 100, 10),
    nsplit    = seq(5, 15, 1),
    splitrule = c("logrank", "logrankscore", "bs.gradient"))
  
  res_list <- lapply(start_id:end_id, function(i) {
    perf <- train_rsf_model(task, hyp[i, ])
    
    data.frame(data      = "FD001",
               ntree     = hyp[i, ]$ntree,
               mtry      = hyp[i, ]$mtry,
               nodesize  = hyp[i, ]$nodesize,
               nodedepth = hyp[i, ]$nodedepth,
               nsplit    = hyp[i, ]$nsplit,
               splitrule = hyp[i, ]$splitrule,
               cindex    = perf$cindex,
               brier     = perf$brier,
               auc       = perf$auc)
  })
  
  res <- do.call(rbind, res_list)
  rownames(res) <- NULL
  write.csv(res, paste0("FD001_id_", start_id, "_", end_id, ".csv"), row.names = FALSE)
}
