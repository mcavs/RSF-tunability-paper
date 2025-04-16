source("install_packages.R")
source("import_datasets.R")
source("task.R")
source("modeling.R")
source("train_rsf_subset.R")
source("tunability_overall.R")
source("tunability_params.R")

task_FD001 <- make_task(data = train_FD001,
                        censor = 250,
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

default_params <- list(
  ntree      = 500,
  mtry       = 3,
  nodesize   = 15,
  nodedepth  = 15,
  nsplit     = 10,
  splitrule  = "logrank"
)

default_perf <- train_rsf_model(task_FD001, default_params)

FD001_all_results <- read_csv("~/Desktop/FD001_all_results.csv")

overall_summary <- tune_overall(default_perf, FD001_all_results, subset_name = "FD001")
print(overall_summary, row.names = FALSE)

parameter_summary <- tune_params(default_perf, FD001_all_results, subset_name = "FD001")
print(parameter_summary, row.names = FALSE)

