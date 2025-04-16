tune_overall <- function(default_perf, all_results, subset_name = "FD001") {
  
  best_cindex <- all_results %>%
    filter(!is.na(cindex)) %>%
    summarise(best = max(cindex)) %>%
    pull(best)
  
  best_brier <- all_results %>%
    filter(!is.na(brier)) %>%
    summarise(best = min(brier)) %>% 
    pull(best)
  
  tunability_cindex <- round(best_cindex - default_perf$cindex, 4)
  tunability_brier  <- round(default_perf$brier - best_brier, 4)
  
  summary <- data.frame(
    subset              = subset_name,
    default_cindex      = round(default_perf$cindex, 4),
    best_cindex         = round(best_cindex, 4),
    tunability_cindex   = tunability_cindex,
    default_brier       = round(default_perf$brier, 4),
    best_brier          = round(best_brier, 4),
    tunability_brier    = tunability_brier
  )
  
  return(summary)
}
