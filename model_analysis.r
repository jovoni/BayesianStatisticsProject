setwd("~/Desktop/UNITS/Bayesian_Statistics/BayesianStatisticsProject")
# 1991 
# Model 1

df = readRDS("data/1991.rds")

fit_v1 = readRDS("models/model_1_fit_1991.rds")

info_att_v1 = rstan::summary(fit_v1, pars=c("att"))
df_att_v1 = as.data.frame(info_att_v1$summary) %>%
  select(-c(se_mean, sd, "25%", "75%", n_eff, Rhat))
rownames(df_att_v1) = levels(as.factor(df$home)) 

info_def_v1 = rstan::summary(fit_v1, pars=c("def"))
df_def_v1 = as.data.frame(info_def_v1$summary) %>%
  select(-c(se_mean, sd, "25%", "75%", n_eff, Rhat))
rownames(df_def_v1) = levels(as.factor(df$home)) 

info_home_v1 = rstan::summary(fit_v1, pars=c("home"))
df_home_v1 = as.data.frame(info_home_v1$summary) %>%
  select(-c(se_mean, sd, "25%", "75%", n_eff, Rhat))


# Model 2

rm(list=ls())
