train_models <- function(task, split) {
  
  # Random Survival Forest (RSF) Model ######################################################
  
  rfsrc_model <- lrn("surv.rfsrc", id = "rfsrc")
  rfsrc_model$train(task, row_ids = split$train)
  
  return(list(
    rfsrc_model = rfsrc_model
  ))
}
