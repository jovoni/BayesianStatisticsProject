# libraries
library(engsoccerdata)
library(dplyr)
source("utils.r")

# data manipulation and saving

d = engsoccerdata::italy
colnames(d)[colnames(d) == 'vgoal'] = 'agoal'

# Year 1991
year = 1991
pts_win = 2

d1991 = season(d, year)
standings1991 = standings(d, year, pts_win)
pts_progression = points_progression(d, year, pts_win)
full_time_counts = full_time_info(d, year)
h_stats = home_stats(d, year, pts_win)
a_stats = away_stats(d, year, pts_win)

dir_alphas_1991 = create_dirichlet_alphas(d, year, 5, pts_win)
att_alpha_1991 = dir_alphas_1991$att
def_alpha_1991 = dir_alphas_1991$def

saveRDS(d1991, "data/1991.rds")
saveRDS(standings1991, "data/1991_standings.rds")
saveRDS(pts_progression, "data/1991_points_progression.rds")
saveRDS(full_time_counts, "data/1991_full_time.rds")
saveRDS(h_stats, "data/1991_home_stats.rds")
saveRDS(a_stats, "data/1991_away_stats.rds")
saveRDS(att_alpha_1991, "data/1991_alpha_dir_att.rds")
saveRDS(def_alpha_1991, "data/1991_alpha_dir_def.rds")

# Year 2007

year = 2007
pts_win = 3

d2007 = season(d, year)
standings2007 = standings(d, year, pts_win)
pts_progression = points_progression(d, year, pts_win)
full_time_counts = full_time_info(d, year)
h_stats = home_stats(d, year, pts_win)
a_stats = away_stats(d, year, pts_win)

dir_alphas_2007 = create_dirichlet_alphas(d, year, 5, pts_win)
att_alpha_2007 = dir_alphas_2007$att
def_alpha_2007 = dir_alphas_2007$def

saveRDS(d2007, "data/2007.rds")
saveRDS(standings2007, "data/2007_standings.rds")
saveRDS(pts_progression, "data/2007_points_progression.rds")
saveRDS(full_time_counts, "data/2007_full_time.rds")
saveRDS(h_stats, "data/2007_home_stats.rds")
saveRDS(a_stats, "data/2007_away_stats.rds")
saveRDS(att_alpha_2007, "data/2007_alpha_dir_att.rds")
saveRDS(def_alpha_2007, "data/2007_alpha_dir_def.rds")

# Year 2020 

year = 2020
pts_win = 3

d2020 = read.csv("data/serie_a_2020.csv")

d2020 = season(d2020, year)
standings2020 = standings(d2020, year, pts_win)
pts_progression = points_progression(d2020, year, pts_win)
full_time_counts = full_time_info(d2020, year)
h_stats = home_stats(d2020, year, pts_win)
a_stats = away_stats(d2020, year, pts_win)

dir_alphas_2020 = create_dirichlet_alphas(d2020, year, 0, pts_win)
att_alpha_2020 = dir_alphas_2020$att
def_alpha_2020 = dir_alphas_2020$def

saveRDS(d2020, "data/2020.rds")
saveRDS(standings2020, "data/2020_standings.rds")
saveRDS(pts_progression, "data/2020_points_progression.rds")
saveRDS(full_time_counts, "data/2020_full_time.rds")
saveRDS(h_stats, "data/2020_home_stats.rds")
saveRDS(a_stats, "data/2020_away_stats.rds")
saveRDS(att_alpha_2020, "data/2020_alpha_dir_att.rds")
saveRDS(def_alpha_2020, "data/2020_alpha_dir_def.rds")

rm(list=ls())
