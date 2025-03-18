data_split <- function(task  = task,
                       ratio = 0.8){
  set.seed(123)
  part <- partition(task, ratio = ratio)  
  return(part)
}