library(dplyr)
library(rstan)
library(bayesplot)
options(mc.cores=2)
setwd("~/Desktop/UNITS/Bayesian_Statistics/BayesianStatisticsProject")

normalize_alpha = function(x) {
  s = sum(x)
  return(x/s)
}

# 1991
# Model 1

d = readRDS("data/1991.rds")

data_model_1 = list(
  N = nrow(d),
  T = length(unique(d$home)),
  hometeam = d$home_id,
  awayteam = d$away_id,
  y1 = d$hgoal,
  y2 = d$agoal
)

model_1 <- stan_model('model_v1.stan')
fit_model_1 <- sampling(
  model_1, data = data_model_1, 
  chains=4
)

saveRDS(fit_model_1, "models/model_1_fit_1991.rds")

home_goal_sim <- as.matrix(fit_model_1, pars="y1_new")
saveRDS(home_goal_sim, "data_sim/1991_goal_home_sim_v1.rds")

away_goal_sim <- as.matrix(fit_model_1, pars="y2_new")
saveRDS(away_goal_sim, "data_sim/1991_goal_away_sim_v1.rds")

remove(away_goal_sim)
remove(home_goal_sim)
remove(model_1)
remove(fit_model_1)
remove(data_model_1)

# Model 2

dir_alpha_att = t(apply(readRDS("data/1991_alpha_dir_att.rds"), 1, normalize_alpha))
dir_alpha_def = t(apply(readRDS("data/1991_alpha_dir_def.rds"), 1, normalize_alpha))

data_model_2 = list(
  N = nrow(d),
  T = length(unique(d$home)),
  hometeam = d$home_id,
  awayteam = d$away_id,
  y1 = d$hgoal,
  y2 = d$agoal,
  dir_alpha_att = dir_alpha_att,
  dir_alpha_def = dir_alpha_def
)

model_2 <- stan_model('model_v2.stan')
fit_model_2 <- sampling(model_2, data = data_model_2, chains=4, iter=8000)

print(fit_model_2, pars = c('pi_attack'))

saveRDS(fit_model_2, "models/model_2_fit_1991.rds")

home_goal_sim <- as.matrix(fit_model_2, pars="y1_new")
saveRDS(home_goal_sim, "data_sim/1991_goal_home_sim_v2.rds")

away_goal_sim <- as.matrix(fit_model_2, pars="y2_new")
saveRDS(away_goal_sim, "data_sim/1991_goal_away_sim_v2.rds")

# Model 3

data_model_3 = list(
  N = nrow(d),
  T = length(unique(d$home)),
  hometeam = d$home_id,
  awayteam = d$away_id,
  y1 = d$hgoal,
  y2 = d$agoal
)

model_3 <- stan_model('model_v3.stan')
fit_model_3 <- sampling(
  model_3, data = data_model_3, 
  chains=4
)

saveRDS(fit_model_3, "models/model_3_fit_1991.rds")

home_goal_sim <- as.matrix(fit_model_3, pars="y1_new")
saveRDS(home_goal_sim, "data_sim/1991_goal_home_sim_v3.rds")

away_goal_sim <- as.matrix(fit_model_3, pars="y2_new")
saveRDS(away_goal_sim, "data_sim/1991_goal_away_sim_v3.rds")

remove(away_goal_sim)
remove(home_goal_sim)
remove(model_3)
remove(fit_model_3)
remove(data_model_3)


# 2007
# Model 1

d = readRDS("data/2007.rds")

data_model_1 = list(
  N = nrow(d),
  T = length(unique(d$home)),
  hometeam = d$home_id,
  awayteam = d$away_id,
  y1 = d$hgoal,
  y2 = d$agoal
)

model_1 <- stan_model('model_v1.stan')
fit_model_1 <- sampling(model_1, data = data_model_1, chains=4, iter=8000)

saveRDS(fit_model_1, "models/model_1_fit_2007.rds")

home_goal_sim <- as.matrix(fit_model_1, pars="y1_new")
saveRDS(home_goal_sim, "data_sim/2007_goal_home_sim_v1.rds")

away_goal_sim <- as.matrix(fit_model_1, pars="y2_new")
saveRDS(away_goal_sim, "data_sim/2007_goal_away_sim_v1.rds")

# Model 2

dir_alpha_att = t(apply(readRDS("data/2007_alpha_dir_att.rds"), 1, normalize_alpha))
dir_alpha_def = t(apply(readRDS("data/2007_alpha_dir_def.rds"), 1, normalize_alpha))

data_model_2 = list(
  N = nrow(d),
  T = length(unique(d$home)),
  hometeam = d$home_id,
  awayteam = d$away_id,
  y1 = d$hgoal,
  y2 = d$agoal,
  dir_alpha_att = dir_alpha_att,
  dir_alpha_def = dir_alpha_def
)

model_2 <- stan_model('model_v2.stan')
fit_model_2 <- sampling(model_2, data = data_model_2, chains=4, iter=10000)

print(fit_model_2, pars = c('home'))

saveRDS(fit_model_2, "models/model_2_fit_2007.rds")

home_goal_sim <- as.matrix(fit_model_2, pars="y1_new")
saveRDS(home_goal_sim, "data_sim/2007_goal_home_sim_v2.rds")

away_goal_sim <- as.matrix(fit_model_2, pars="y2_new")
saveRDS(away_goal_sim, "data_sim/2007_goal_away_sim_v2.rds")


# 2020
# Model 1

d = readRDS("data/2020.rds")

data_model_1 = list(
  N = nrow(d),
  T = length(unique(d$home)),
  hometeam = d$home_id,
  awayteam = d$away_id,
  y1 = d$hgoal,
  y2 = d$agoal
)

model_1 <- stan_model('model_v1.stan')
fit_model_1 <- sampling(model_1, data = data_model_1, chains=4, iter=8000)

saveRDS(fit_model_1, "models/model_1_fit_2020.rds")

home_goal_sim <- as.matrix(fit_model_1, pars="y1_new")
saveRDS(home_goal_sim, "data_sim/2020_goal_home_sim_v1.rds")

away_goal_sim <- as.matrix(fit_model_1, pars="y2_new")
saveRDS(away_goal_sim, "data_sim/2020_goal_away_sim_v1.rds")

# Model 2

dir_alpha_att = readRDS("data/2020_alpha_dir_att.rds")
dir_alpha_def = readRDS("data/2020_alpha_dir_def.rds")

data_model_2 = list(
  N = nrow(d),
  T = length(unique(d$home)),
  hometeam = d$home_id,
  awayteam = d$away_id,
  y1 = d$hgoal,
  y2 = d$agoal,
  dir_alpha_att = dir_alpha_att,
  dir_alpha_def = dir_alpha_def
)

model_2 <- stan_model('model_v2.stan')
fit_model_2 <- sampling(model_2, data = data_model_2, chains=4, iter=8000)

saveRDS(fit_model_2, "models/model_2_fit_2020.rds")

home_goal_sim <- as.matrix(fit_model_2, pars="y1_new")
saveRDS(home_goal_sim, "data_sim/2020_goal_home_sim_v2.rds")

away_goal_sim <- as.matrix(fit_model_2, pars="y2_new")
saveRDS(away_goal_sim, "data_sim/2020_goal_away_sim_v2.rds")

rm(list=ls())
