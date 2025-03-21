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

