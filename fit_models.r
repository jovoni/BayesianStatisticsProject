library(dplyr)
library(rstan)
library(bayesplot)
options(mc.cores=4)
setwd("~/Desktop/UNITS/Bayesian_Statistics/BayesianStatisticsProject")

# 1991
# Model 1

d = readRDS("data/seriea1991.rds")

data_model_1 = list(
  N = nrow(d),
  T = length(unique(d$home)),
  hometeam = d$home_id,
  awayteam = d$away_id,
  y1 = d$hgoal,
  y2 = d$agoal
)

model_1 <- stan_model('model_v1.stan')
fit_model_1 <- sampling(model_1, data = data_model_1)

saveRDS(fit_model_1, "models/model_1_fit_1991.rds")

rm(list=ls())
