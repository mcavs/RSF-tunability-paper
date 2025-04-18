tune_params <- function(default_perf, all_results, subset_name = "FD001") {
  
  default_cindex <- default_perf$cindex
  default_brier  <- default_perf$brier
  
  make_tunability_row <- function(filtered_data, param_name) {
    best_cindex <- max(filtered_data$cindex, na.rm = TRUE)
    best_brier  <- min(filtered_data$brier, na.rm = TRUE)
    
    data.frame(
      parameter = param_name,
      subset = subset_name,
      default_cindex    = round(default_cindex, 4),
      best_cindex       = round(best_cindex, 4),
      tunability_cindex = round(best_cindex - default_cindex, 4),
      default_brier     = round(default_brier, 4),
      best_brier        = round(best_brier, 4),
      tunability_brier  = round(default_brier - best_brier, 4)
    )
  }
  
  tunability_ntree <- all_results %>%
    filter(mtry == 3, nodesize == 15, nodedepth == 15, nsplit == 10, trimws(splitrule) == "logrank", ntree != 500) %>%
    make_tunability_row("ntree")
  
  tunability_mtry <- all_results %>%
    filter(ntree == 500, nodesize == 15, nodedepth == 15, nsplit == 10, trimws(splitrule) == "logrank", mtry != 3) %>%
    make_tunability_row("mtry")
  
  tunability_nodesize <- all_results %>%
    filter(ntree == 500, mtry == 3, nodedepth == 15, nsplit == 10, trimws(splitrule) == "logrank", nodesize != 15) %>%
    make_tunability_row("nodesize")
  
  tunability_nodedepth <- all_results %>%
    filter(ntree == 500, mtry == 3, nodesize == 15, nsplit == 10, trimws(splitrule) == "logrank", nodedepth != 15) %>%
    make_tunability_row("nodedepth")
  
  tunability_nsplit <- all_results %>%
    filter(ntree == 500, mtry == 3, nodesize == 15, nodedepth == 15, trimws(splitrule) == "logrank", nsplit != 10) %>%
    make_tunability_row("nsplit")
  
  tunability_splitrule <- all_results %>%
    filter(ntree == 500, mtry == 3, nodesize == 15, nodedepth == 15, nsplit == 10, trimws(splitrule) != "logrank") %>%
    make_tunability_row("splitrule")
  
  tunability_summary <- bind_rows(
    tunability_ntree,
    tunability_mtry,
    tunability_nodesize,
    tunability_nodedepth,
    tunability_nsplit,
    tunability_splitrule
  )
  
  return(tunability_summary)
}

param_FD001 <- tune_params(FD001_default, filter(all_results, data == "FD001"), "FD001")
param_FD002 <- tune_params(FD002_default, filter(all_results, data == "FD002"), "FD002")
param_FD003 <- tune_params(FD003_default, filter(all_results, data == "FD003"), "FD003")
param_FD004 <- tune_params(FD004_default, filter(all_results, data == "FD004"), "FD004")

param_summary_all <- bind_rows(param_FD001, param_FD002, param_FD003, param_FD004)
print(param_summary_all)
