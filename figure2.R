all_results <- rbind(FD001_all_results,
                     FD002_all_results,
                     FD003_all_results,
                     FD004_all_results)

library(dplyr)
library(tidyr)
library(RColorBrewer)
library(scales)
library(ggplot2)

long_results <- FD001_all_results |>
  pivot_longer(cols = c(ntree, mtry, nodesize, nodedepth, nsplit), 
               names_to = "hyp", 
               values_to = "hval") |>
  rename(dind = data)

library(ggplot2)
library(RColorBrewer)
library(scales)

ggplot(long_results, aes(x = hval, y = dind, color = cindex)) +
  geom_point(size = 3) + 
  facet_wrap(~hyp, ncol = 2, scales = "free_x") +  
  theme_bw() + 
  labs(x = "", y = "", color = "") + 
  scale_color_gradientn(
    colors  = brewer.pal(7, "YlGnBu"),
    limits  = c(0.5, 1),
    values  = rescale(c(0.5, 0.75, 1), from = c(0.5, 1)),
    breaks  = c(0.5, 0.75, 1),
    labels  = c("0.5", "0.75", "1"),
    oob     = squish  
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
