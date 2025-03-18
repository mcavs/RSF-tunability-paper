install.packages("readr")
install.packages("dplyr")
install.packages("remotes"); library(remotes)
remotes::install_github("mlr-org/mlr3proba", force = TRUE)
remotes::install_github("mlr-org/mlr3extralearners", force = TRUE)
install.packages("ggplot2")
install.packages("survAUC")

library(readr)
library(dplyr)
library(mlr3proba)
library(ggplot2)
library(survAUC)
library(mlr3extralearners)


# Sadece Random Survival Forest (RSF) modelini yükleme
install_learners(c("surv.rfsrc"))