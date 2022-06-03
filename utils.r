# functions preprocessing

season = function(d, year) {
  if (!is.null(year))
    d = d %>%
      filter(Season == year)
  
  d_season = data.frame(
    Date = d$Date,
    Season = d$Season,
    home = d$home,
    visitor = d$visitor,
    hgoal = d$hgoal,
    agoal = d$agoal,
    home_id = as.numeric(as.factor(d$home)),
    away_id = as.numeric(as.factor(d$visitor))
  )
  
  return(d_season)
}

simulated_season = function(real_season_df, sim_goal_home, sim_goal_away) {
  mean_goal_home = round(colMedians(sim_goal_home), 0)
  mean_goal_away = round(colMedians(sim_goal_away), 0)
  
  season_sim_df = data.frame(
    Date = real_season_df$Date,
    Season = real_season_df$Season,
    home = real_season_df$home,
    visitor = real_season_df$visitor,
    home_id = real_season_df$home_id,
    away_id = real_season_df$away_id,
    hgoal = mean_goal_home,
    agoal = mean_goal_away
  )
  rownames(season_sim_df) <- c(1:nrow(real_season_df))
  
  return(season_sim_df)
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
    group_by(visitor) %>%
    rename(team = visitor) %>%
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

add_att_and_def_ranking_to_standings = function(d) {
  # top 5 attackers/defenders gets 3
  # lower 5 attackers/defenders gets 1
  att_rank = c()
  def_rank = c()
  
  att_sorted = d[order(-d$scored),]$team
  top_att = head(att_sorted, 5)
  bottom_att = tail(att_sorted, 5)
  
  def_sorted = att_sorted = d[order(d$conceded),]$team
  top_def = head(def_sorted, 5)
  bottom_def = tail(def_sorted, 5)
  
  for (i in 1:nrow(d)) {
    t  = d$team[i]
    if (t %in% top_att) {
      att_rank[i] = 3
    } else if (t %in% bottom_att) {
      att_rank[i] = 1
    } else {
      att_rank[i] = 2
    }
    
    if (t %in% top_def) {
      def_rank[i] = 3
    } else if (t %in% bottom_def) {
      def_rank[i] = 1
    } else {
      def_rank[i] = 2
    }
  }
  
  res = data.frame(
    team = d$team,
    att_rank = att_rank,
    def_rank = def_rank
  )
  
  return(res)
}

create_dirichlet_alphas = function(d, year, n_years, pts_per_win) {
  d_year = season(d, year)
  teams = unique(d_year$home)
  n_teams = length(teams)
  idx_teams = as.numeric(as.factor(teams))
  
  print(teams)
  
  mat_att = matrix(1, nrow=n_teams, ncol=3)
  mat_def = matrix(1, nrow=n_teams, ncol=3)
  
  if (n_years >= 1) {
    for (i in 1:n_years) {
      s_year = standings(d, year - i, pts_per_win)
      rank_y = add_att_and_def_ranking_to_standings(s_year)
      
      for (t in teams) {
        i = d_year[d_year$home == t,]$home_id[1]
        if (t %in% rank_y$team) {
          att_rank = rank_y[rank_y$team == t,]$att_rank
          def_rank = rank_y[rank_y$team == t,]$def_rank
          
          mat_att[i, att_rank] =  mat_att[i, att_rank] + 1
          mat_def[i, def_rank] =  mat_def[i, def_rank] + 1
        }
      }
    }
  }
  res = list("att" = mat_att, "def" = mat_def)
  return(res)
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
  dy = dy[order(dy$Date, decreasing = FALSE), ]
  
  team_names = unique(dy$home)
  n_of_teams = length(team_names)
  pts_progression = data.frame(matrix(ncol=n_of_teams, nrow=(n_of_teams-1)*2))
  colnames(pts_progression) = team_names
  for (t in team_names) {
    team_d = dy %>%
      filter(home == t | visitor == t) %>%
      mutate(at_home=ifelse(home==t, 1, 0)) %>%
      mutate(goal_diff=ifelse(at_home==1, hgoal - agoal, agoal - hgoal)) %>%
      mutate(point=ifelse(goal_diff>0, point_per_victory, ifelse(goal_diff==0, 1, 0)))
    
    pts_progression[,t] = cumsum(team_d$point)
  }
  return(pts_progression)
}