tune_params <- function(all_results, subset_name = "FD001") {
  
  tunability_ntree <- all_results |>
    filter(mtry == 3, nodesize == 15, nodedepth == 15, nsplit == 10, trimws(splitrule) == "logrank") |>
    group_by(data) |>
    mutate(
      ref_cindex = cindex[ntree == 500],     
      diff_cindex = cindex - ref_cindex,
      ref_brier = brier[ntree == 500],     
      diff_brier = brier - ref_brier) |>
    ungroup() |>
    select(data, 
           ntree, 
           cindex, 
           ref_cindex, 
           diff_cindex, 
           brier, 
           ref_brier, 
           diff_brier) |>
    pivot_longer(cols      = c(ntree), 
                 names_to  = "hyp", 
                 values_to = "hval")
  
  tunability_mtry <- all_results |>
    filter(ntree == 500, nodesize == 15, nodedepth == 15, nsplit == 10, trimws(splitrule) == "logrank") |>
    group_by(data) |>
    mutate(
      ref_cindex = cindex[mtry == 3],     
      diff_cindex = cindex - ref_cindex,
      ref_brier = brier[mtry == 3],     
      diff_brier = brier - ref_brier) |>
    ungroup() |>
    select(data, 
           mtry, 
           cindex, 
           ref_cindex, 
           diff_cindex, 
           brier, 
           ref_brier, 
           diff_brier) |>
    pivot_longer(cols      = c(mtry), 
                 names_to  = "hyp", 
                 values_to = "hval")
  
  tunability_nodesize <- all_results |>
    filter(ntree == 500, mtry == 3, nodedepth == 15, nsplit == 10, trimws(splitrule) == "logrank") |>
    group_by(data) |>
    mutate(
      ref_cindex = cindex[nodesize == 15],     
      diff_cindex = cindex - ref_cindex,
      ref_brier = brier[nodesize == 15],     
      diff_brier = brier - ref_brier) |>
    ungroup() |>
    select(data, 
           nodesize, 
           cindex, 
           ref_cindex, 
           diff_cindex, 
           brier, 
           ref_brier, 
           diff_brier) |>
    pivot_longer(cols      = c(nodesize), 
                 names_to  = "hyp", 
                 values_to = "hval")
  
  tunability_nodedepth <- all_results |>
    filter(ntree == 500, mtry == 3, nodesize == 15, nsplit == 10, trimws(splitrule) == "logrank") |>
    group_by(data) |>
    mutate(
      ref_cindex = cindex[nodedepth == 15],     
      diff_cindex = cindex - ref_cindex,
      ref_brier = brier[nodedepth == 15],     
      diff_brier = brier - ref_brier) |>
    ungroup() |>
    select(data, 
           nodedepth, 
           cindex, 
           ref_cindex, 
           diff_cindex, 
           brier, 
           ref_brier, 
           diff_brier) |>
    pivot_longer(cols      = c(nodedepth), 
                 names_to  = "hyp", 
                 values_to = "hval")
  
  tunability_nsplit <- all_results |>
    filter(ntree == 500, mtry == 3, nodesize == 15, nodedepth == 15, trimws(splitrule) == "logrank") |>
    group_by(data) |>
    mutate(
      ref_cindex = cindex[nsplit == 10],     
      diff_cindex = cindex - ref_cindex,
      ref_brier = brier[nsplit == 10],     
      diff_brier = brier - ref_brier) |>
    ungroup() |>
    select(data, 
           nsplit, 
           cindex, 
           ref_cindex, 
           diff_cindex, 
           brier, 
           ref_brier, 
           diff_brier) |>
    pivot_longer(cols      = c(nsplit), 
                 names_to  = "hyp", 
                 values_to = "hval")
  
  tunability_splitrule <- all_results |>
    filter(ntree == 500, mtry == 3, nodesize == 15, nodedepth == 15, nsplit == 10) |>
    group_by(data) |>
    mutate(
      ref_cindex = cindex[splitrule == "logrank"],     
      diff_cindex = cindex - ref_cindex,
      ref_brier = brier[splitrule == "logrank"],     
      diff_brier = brier - ref_brier) |>
    ungroup() |>
    select(data, 
           splitrule, 
           cindex, 
           ref_cindex, 
           diff_cindex, 
           brier, 
           ref_brier, 
           diff_brier) |>
    pivot_longer(cols      = c(splitrule), 
                 names_to  = "hyp", 
                 values_to = "hval") |>
    mutate(hval = ifelse(hval == "bs.gradient", 1, ifelse(hval == "logrank", 2, 3)))
    
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

param_FD001 <- tune_params(filter(all_results, data == "FD001"), "FD001")
param_FD002 <- tune_params(filter(all_results, data == "FD002"), "FD002")
param_FD003 <- tune_params(filter(all_results, data == "FD003"), "FD003")
param_FD004 <- tune_params(filter(all_results, data == "FD004"), "FD004")

param_summary_all <- bind_rows(param_FD001, param_FD002, param_FD003, param_FD004)
print(param_summary_all)

############################################################################################################################################################

library(ggplot2)
library(RColorBrewer)
library(scales)

ggplot(param_summary_all, aes(x = as.factor(hval), y = data, fill = diff_cindex)) +
  geom_tile() + 
  facet_wrap(~hyp, ncol = 2, scales = "free_x") +
  theme_bw() + 
  labs(x = "", y = "", fill = "") + 
  scale_fill_gradientn(
    colors   = brewer.pal(7, "Greys"),
    limits   = c(0, 0.05),
    breaks   = c(0, 0.01, 0.02, 0.03, 0.04, 0.05),
    labels   = c("0", "0.01", "0.02", "0.03", "0.04", "0.05"),
    oob      = squish
  ) +
  theme(
    axis.text.x      = element_text(size = 10),
    axis.text.y      = element_text(family = "Courier", size = 10),
    axis.title       = element_text(size = 15),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.width = unit(2, "cm"),
    strip.text.x     = element_text(family = "Courier", size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  
