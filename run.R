source("install_packages.R")
source("import_datasets.R")
source("task.R")
source("modeling.R")
source("train_rsf_subset.R")
source("tunability_overall.R")
source("tunability_params.R")

task_FD001 <- make_task(data = train_FD001,
                        censor = 250,
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

default_params <- list(
  ntree      = 500,
  mtry       = 3,
  nodesize   = 15,
  nodedepth  = 15,
  nsplit     = 10,
  splitrule  = "logrank"
)

default_perf <- train_rsf_model(task_FD001, default_params)

all_results <- read_csv("all_results.csv")

overall_summary_all <- bind_rows(overall_FD001, overall_FD002, overall_FD003, overall_FD004)
print(overall_summary_all)

param_summary_all <- bind_rows(param_FD001, param_FD002, param_FD003, param_FD004)
print(param_summary_all)



# Grafik

library(dplyr)
library(tidyr)
library(RColorBrewer)
library(scales)

long_results <- all_results %>%
  pivot_longer(cols = c(ntree, mtry, nodesize, nodedepth, nsplit), 
               names_to = "hyp", 
               values_to = "hval") %>%
  rename(dind = data)

ggplot(long_results, aes(x = hval, y = dind, color = cindex)) +
  geom_point(size = 3, alpha = 0.5) + 
  facet_wrap(~hyp, ncol = 2, scales = "free_x") +  # her facet için ayrı x ekseni
  theme_bw() + 
  labs(x = "", y = "", color = "") + 
  scale_color_gradientn(
    colors = brewer.pal(7, "YlGnBu"),
    values = rescale(c(0, 0.5, 1)),  
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "0.25", "0.5", "0.75", "1")
  ) +
  theme(
    axis.text.x      = element_text(size = 12),
    axis.text.y      = element_text(family = "Courier", size = 12),
    axis.title       = element_text(size = 15),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.width = unit(2, "cm"), 
    strip.text.x     = element_text(family = "Courier", size = 15)
  )




