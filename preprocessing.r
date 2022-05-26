# libraries
library(engsoccerdata)
library(dplyr)

# functions
season = function(df, year) {
  d = d %>%
    filter(Season == year)
  
  date = d$Date
  home = d$home
  away = d$visitor
  hgoal = d$hgoal
  agoal = d$vgoal
  home_id = as.numeric(as.factor(d$home))
  away_id = as.numeric(as.factor(d$visitor))
  
  d_season = data.frame(date, home, away, home_id, away_id, hgoal, agoal)
  return(d_season)
}

home_stats = function(df, year, point_for_victory) {
  dy = season(df, year)
  
  home_stat = dy %>%
    group_by(home) %>%
    rename(team = home) %>%
    summarise(
      pts_home = sum(hgoal > agoal) * point_for_victory + sum(hgoal == agoal),
      home_wins = sum(hgoal > agoal),
      home_ties = sum(hgoal == agoal),
      home_losses = sum(hgoal < agoal),
      home_scored = sum(hgoal),
      home_conceded = sum(agoal),
      home_goal_diff = home_scored - home_conceded
    )
  return(home_stat)
}

away_stats = function(df, year, point_for_victory) {
  dy = season(df, year)
  
  away_stats = dy %>%
    group_by(away) %>%
    rename(team = away) %>%
    summarise(
      pts_away = sum(agoal > hgoal) * point_for_victory + sum(agoal == hgoal),
      away_wins = sum(agoal > hgoal),
      away_ties = sum(agoal == hgoal),
      away_losses = sum(agoal < hgoal),
      away_scored = sum(agoal),
      away_conceded = sum(hgoal),
      away_goal_diff = away_scored - away_conceded
    )
  return(away_stats)
}

standings = function(df, year, point_for_victory) {
  home_stat = home_stats(df, year, point_for_victory)
  away_stat = away_stats(df, year, point_for_victory)
  
  general_stats = home_stat %>%
    left_join(away_stat, by="team") %>%
    #left_join(home_stats, away_stats, by="team") %>%
    group_by(team, .drop=FALSE) %>%
    summarise(
      points = pts_home + pts_away,
      wins = away_wins + home_wins,
      ties = away_ties + home_ties,
      losses = away_losses + home_losses,
      scored = away_scored + home_scored,
      conceded = away_conceded + home_conceded,
      goal_diff = scored - conceded
    )
  general_stats = general_stats[order(general_stats$points, decreasing = TRUE),]  
  return(general_stats)
}

full_time_info = function(d, year) {
  d = season(d, year)
  max_goal = max(max(d$agoal), max(d$hgoal))
  goals_home = c(0:max_goal)
  goals_away = c(0:max_goal)
  hm = expand.grid(goal_home = goals_home, goal_away = goals_away)
  full_time_counts = c()
  for (i in 1:nrow(hm)) {
    g_home = hm[i,"goal_home"]
    g_away = hm[i,"goal_away"]
    f = d %>%
      filter(d$agoal == g_away & d$hgoal == g_home)
    full_time_counts[i] = nrow(f) 
  }
  hm$full_time_count = full_time_counts
  hm <- hm %>%
    mutate(goal_home = factor(goal_home),
      goal_away = factor(goal_away, levels=rev(unique(goal_away))))
  return(hm)
}

points_progression = function(d, year, point_per_victory) {
  dy = season(d, year)
  dy = dy[order(dy$date, decreasing = FALSE), ]
  
  team_names = unique(dy$home)
  n_of_teams = length(team_names)
  pts_progression = data.frame(matrix(ncol=n_of_teams, nrow=(n_of_teams-1)*2))
  colnames(pts_progression) = team_names
  for (t in team_names) {
    team_d = dy %>%
      filter(home == t | away == t) %>%
      mutate(at_home=ifelse(home==t, 1, 0)) %>%
      mutate(goal_diff=ifelse(at_home==1, hgoal - agoal, agoal - hgoal)) %>%
      mutate(point=ifelse(goal_diff>0, pts_win, ifelse(goal_diff==0, 1, 0)))
    
    pts_progression[,t] = cumsum(team_d$point)
  }
  return(pts_progression)
}

# data manipulation and saving

d = engsoccerdata::italy

year = 1991
pts_win = 2

d1991 = season(d, year)
standings1991 = standings(d, year, pts_win)
pts_progression = points_progression(d, year, pts_win)
full_time_counts = full_time_info(d, year)
h_stats = home_stats(d, year, pts_win)
a_stats = away_stats(d, year, pts_win)

saveRDS(d1991, "data/1991.rds")
saveRDS(standings1991, "data/1991_standings.rds")
saveRDS(pts_progression, "data/1991_points_progression.rds")
saveRDS(full_time_counts, "data/1991_full_time.rds")
saveRDS(h_stats, "data/1991_home_stats.rds")
saveRDS(a_stats, "data/1991_away_stats.rds")

rm(list=ls())