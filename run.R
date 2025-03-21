source("install_packages.R")
source("import_datasets.R")
source("task.R")
source("data_split.R")
source("modeling.R")
source("performance.R")

train_task <- make_task(data = train_FD001)

split <- data_split(task = train_task)

rsf_result <- train_rsf_model(
  task      = train_task, 
  ntree     = 500, 
  mtry      = 20, 
  nodesize  = 25, 
  nodedepth = 10, 
  nsplit    = 10,
  splitrule = "logrank"
)

print(paste("C-Index:", round(rsf_result$c_index, 4)))


