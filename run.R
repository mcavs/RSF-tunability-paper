source("install_packages.R")
source("import_datasets.R")
source("task.R")
source("data_split.R")
source("modeling.R")

task_FD001 <- make_task(data      = train_FD001,
                        censor    = 250,
                        variables = c("unit_ID", 
                                      "RUL",
                                      "operational_setting_3",
                                      "sensor_1",
                                      "sensor_6",
                                      "sensor_10",
                                      "sensor_16",
                                      "sensor_18",
                                      "sensor_19",
                                      "sensor_5"))

hyp <- expand.grid(
  ntree     = seq(100, 2000, 200),
  mtry      = seq(1:10),
  nodesize  = seq(5, 100, 10),
  nodedepth = seq(5, 100, 10),
  nsplit    = seq(5, 15, 1),
  splitrule = c("logrank", "logrankscore", "bs.gradient"))

start <- Sys.time()
res <- data.frame() 
for(i in 1:dim(hyp)[1]){
  perf <- train_rsf_model(task_FD001, hyp[i, ])
  
  new_row <- data.frame(data      = "FD001",
                        ntree     = hyp[i, ]$ntree,
                        mtry      = hyp[i, ]$mtry,
                        nodesize  = hyp[i, ]$nodesize,
                        nodedepth = hyp[i, ]$nodedepth,
                        nsplit    = hyp[i, ]$nsplit,
                        splitrule = hyp[i, ]$splitrule,
                        cindex    = perf$cindex,
                        brier     = perf$brier,
                        auc       = perf$auc)
  
  res           <- rbind(res, new_row)
  rownames(res) <- NULL
}
stop <- Sys.time()