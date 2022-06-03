source("utils.r")
library(matrixStats)
setwd("~/Desktop/UNITS/Bayesian_Statistics/BayesianStatisticsProject")

# 1991
# Model 1

home_goal_sim = readRDS("data_sim/1991_goal_home_sim_v1.rds")
away_goal_sim = readRDS("data_sim/1991_goal_away_sim_v1.rds")

real_season = readRDS("data/1991.rds")
real_pts_progression = readRDS("data/1991_points_progression.rds")

sim_season = simulated_season(real_season_df = real_season, sim_goal_home = home_goal_sim, sim_goal_away = away_goal_sim)
sim_pts_progression = points_progression(d=sim_season, year=1991, point_per_victory=2) 

saveRDS(sim_pts_progression, "data_sim/1991_points_progression_sim_v1.rds")

# Model 2

home_goal_sim = readRDS("data_sim/1991_goal_home_sim_v2.rds")
away_goal_sim = readRDS("data_sim/1991_goal_away_sim_v2.rds")

real_season = readRDS("data/1991.rds")
real_pts_progression = readRDS("data/1991_points_progression.rds")

sim_season = simulated_season(real_season_df = real_season, sim_goal_home = home_goal_sim, sim_goal_away = away_goal_sim)
sim_pts_progression = points_progression(d=sim_season, year=1991, point_per_victory=2) 

saveRDS(sim_pts_progression, "data_sim/1991_points_progression_sim_v2.rds")

# Model 3

home_goal_sim = readRDS("data_sim/1991_goal_home_sim_v3.rds")
away_goal_sim = readRDS("data_sim/1991_goal_away_sim_v3.rds")

real_season = readRDS("data/1991.rds")
real_pts_progression = readRDS("data/1991_points_progression.rds")

sim_season = simulated_season(real_season_df = real_season, sim_goal_home = home_goal_sim, sim_goal_away = away_goal_sim)
sim_pts_progression = points_progression(d=sim_season, year=1991, point_per_victory=2) 

saveRDS(sim_pts_progression, "data_sim/1991_points_progression_sim_v3.rds")

# 2007

# Model 1

home_goal_sim = readRDS("data_sim/2007_goal_home_sim_v1.rds")
away_goal_sim = readRDS("data_sim/2007_goal_away_sim_v1.rds")

real_season = readRDS("data/2007.rds")
real_pts_progression = readRDS("data/2007_points_progression.rds")

sim_season = simulated_season(real_season_df = real_season, sim_goal_home = home_goal_sim, sim_goal_away = away_goal_sim)
sim_pts_progression = points_progression(d=sim_season, year=2007, point_per_victory=3) 

saveRDS(sim_pts_progression, "data_sim/2007_points_progression_sim_v1.rds")

# Model 2

home_goal_sim = readRDS("data_sim/2007_goal_home_sim_v2.rds")
away_goal_sim = readRDS("data_sim/2007_goal_away_sim_v2.rds")

real_season = readRDS("data/2007.rds")
real_pts_progression = readRDS("data/2007_points_progression.rds")

sim_season = simulated_season(real_season_df = real_season, sim_goal_home = home_goal_sim, sim_goal_away = away_goal_sim)
sim_pts_progression = points_progression(d=sim_season, year=2007, point_per_victory=3) 

saveRDS(sim_pts_progression, "data_sim/2007_points_progression_sim_v2.rds")

rm(list=ls())
