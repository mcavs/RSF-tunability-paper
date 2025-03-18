make_task <- function(data      = train_FD001,
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
                                    "sensor_5")
){
  data <- data |>
    group_by(unit_ID) |>
    filter(cycles == censor | cycles == max(cycles)) |>
    slice_min(order_by = abs(cycles - censor)) |>
    ungroup() 
  
  task <- TaskSurv$new(
    id      = "task",
    backend = data |> select(-one_of(variables)),        
    time    = "cycles",         
    event   = "event"      
  )
  return(task)
}