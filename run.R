source("install_packages.R")
source("import_datasets.R")
source("task.R")
source("modeling.R")
source("train_rsf_subset.R")

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

# örnek: train_rsf_subset(task_FD001, 1, 10)



default_params <- list(
  ntree      = 500,
  mtry       = 3,
  nodesize   = 15,
  nodedepth  = 100,
  nsplit     = 10,
  splitrule  = "logrank"
)
default_perf <- train_rsf_model(task_FD001, default_params)

FD001_all_results <- read_csv("FD001_all_results.csv")

best_cindex <- FD001_all_results %>%
  filter(!is.na(cindex)) %>%
  summarise(best = max(cindex)) %>%
  pull(best)

best_brier <- FD001_all_results %>%
  filter(!is.na(brier)) %>%
  summarise(best = min(brier)) %>% 
  pull(best)

tunability_cindex <- round(best_cindex - default_perf$cindex, 4)
tunability_brier <- round(default_perf$brier - best_brier, 4)

tunability_summary <- data.frame(
  subset              = "FD001",
  default_cindex      = round(default_perf$cindex, 4),
  best_cindex         = round(best_cindex, 4),
  tunability_cindex   = tunability_cindex,
  default_brier       = round(default_perf$brier, 4),
  best_brier          = round(best_brier, 4),
  tunability_brier    = tunability_brier
)

print(tunability_summary)







