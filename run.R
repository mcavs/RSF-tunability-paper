source("install_packages.R")
source("import_datasets.R")
source("task.R")
source("data_split.R")
source("modeling.R")
source("performance.R")

task <- make_task(data = train_FD001)
split <- data_split(task = task)

models <- train_models(task, split)

performance_results <- measure_performance(models, task, split)

print("Modelin Varsayılan Performansı:")
print(paste("C-Index:", round(performance_results$c_index, 4)))
print(paste("Brier Score:", round(performance_results$brier, 4)))
print(paste("Uno AUC:", round(performance_results$auc, 4)))


