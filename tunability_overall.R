all_results <- read_csv("~/Desktop/all_results.csv")

FD001_default <- list(cindex = 0.7214, brier = 0.1422)
FD002_default <- list(cindex = 0.6658, brier = 0.1484)
FD003_default <- list(cindex = 0.8190, brier = 0.1157)
FD004_default <- list(cindex = 0.7634, brier = 0.1309)

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

overall_FD001 <- tune_overall(FD001_default, filter(all_results, data == "FD001"), "FD001")
overall_FD002 <- tune_overall(FD002_default, filter(all_results, data == "FD002"), "FD002")
overall_FD003 <- tune_overall(FD003_default, filter(all_results, data == "FD003"), "FD003")
overall_FD004 <- tune_overall(FD004_default, filter(all_results, data == "FD004"), "FD004")

overall_summary_all <- bind_rows(overall_FD001, overall_FD002, overall_FD003, overall_FD004)
print(overall_summary_all)







