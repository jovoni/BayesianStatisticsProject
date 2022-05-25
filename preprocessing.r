# libraries
library(engsoccerdata)
library(dplyr)

# functions
season = function(df, year) {
  d = d %>%
    filter(Season == year)
  
  home = d$home
  away = d$visitor
  hgoal = d$hgoal
  agoal = d$vgoal
  home_id = as.numeric(as.factor(d$home))
  away_id = as.numeric(as.factor(d$visitor))
  
  d_season = data.frame(home, away, home_id, away_id, hgoal, agoal)
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

d = engsoccerdata::italy

year = 1991
pts_win = 2

d1991 = season(d, year)
standings1991 = standings(d, year, pts_win)



rm(d)
