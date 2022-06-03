library(ggplot2)
library(RColorBrewer)
library(tibble)
setwd("~/Desktop/UNITS/Bayesian_Statistics/BayesianStatisticsProject")

save_img = function(image_name, image) {
  png(file=image_name, width=1200, height=700)
  image
  dev.off() 
}

# 1991

d = readRDS("data/1991.rds")
standings = readRDS("data/1991_standings.rds")
pts_prog = readRDS("data/1991_points_progression.rds")
sim_pts_prog_1 = readRDS("data_sim/1991_points_progression_sim_v1.rds")
sim_pts_prog_2 = readRDS("data_sim/1991_points_progression_sim_v2.rds")
full_time = readRDS("data/1991_full_time.rds")
#home_stats = readRDS("data/1991_home_stats.rds")
#away_stats = readRDS("data/1991_away_stats.rds")


best_attack_plot = ggplot(data=standings, aes(x=reorder(team, scored), y=scored, fill=scored)) +
  geom_bar(stat="identity") + 
  scale_fill_gradient(low="yellow", high="green") +
  ggtitle(label="Which team had the best attack?", subtitle = "Ranking of teams by scored goals in Serie A 1991-92") +
  labs(x="", y="Goals scored") + 
  geom_text(aes(label=scored), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(plot.title=element_text(size=24, face="bold"), axis.text=element_text(size=14), axis.title=element_text(size=14), plot.subtitle=element_text(size=20)) +
  coord_flip() + guides(fill="none")

worst_defense_plot = ggplot(data=standings, aes(x=reorder(team, -conceded), y=conceded, fill=conceded)) +
  geom_bar(stat="identity") + 
  scale_fill_gradient(low="yellow", high="red") +
  ggtitle(label="Which team had the bests defense?", subtitle = "Ranking of teams by conceded goals in Serie A 1991-92") +
  labs(x ="",y="Goals conceded") + 
  geom_text(aes(label=conceded), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(plot.title=element_text(size=24, face="bold"), axis.text=element_text(size=14), axis.title=element_text(size=14), plot.subtitle=element_text(size=20)) +
  coord_flip() + guides(fill="none")

home_goal_dist = ggplot(data=d, aes(x=factor(hgoal))) + 
  geom_bar(position="stack", color="darkorange", fill="darkorange") +
  ggtitle("Home goals", subtitle="Distribution of home goals in Serie A 1991-92") +
  theme(plot.title=element_text(size=24, face="bold"), axis.text=element_text(size=14), axis.title=element_text(size=14), plot.subtitle=element_text(size=20)) +
  labs(x="Goals", y="Count") + guides(fill="none")

away_goal_dist = ggplot(data=d, aes(x=factor(agoal))) + 
  geom_bar(position="stack", color="darkorange", fill="darkorange") +
  ggtitle("Away goals", subtitle="Distribution of away goals in Serie A 1991-92") +
  theme(plot.title=element_text(size=24, face="bold"), axis.text=element_text(size=14), axis.title=element_text(size=14), plot.subtitle=element_text(size=20)) +
  labs(x="Goals", y="Count") + guides(fill="none")

full_time_results_hm = ggplot(full_time, aes(goal_home, goal_away, fill=full_time_count)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  theme_bw() + coord_equal() + guides(fill="none") +
  geom_text(aes(label=full_time_count), color="black", size=6) + 
  scale_fill_distiller(palette="Blues", direction=1) + 
  theme(plot.title=element_text(size=24, face="bold"), axis.text=element_text(size=14), axis.title=element_text(size=14), plot.subtitle=element_text(size=20)) +
  labs(x="Home goal", y="Away goal") + ggtitle("Full time results counted", subtitle="Which are the most common results in Serie A 1991-92?")

x = c(1:nrow(pts_prog))
pts_prog$model = "real_values"
sim_pts_prog_1$model = "model_v1"
sim_pts_prog_2$model = "model_v2"
pts_prog$game = x
sim_pts_prog_1$game = x
sim_pts_prog_2$game = x

all_points_progression = rbind(pts_prog, sim_pts_prog_1, sim_pts_prog_2)
all_points_progression = all_points_progression %>%
  reshape2::melt(id=c("model", "game"))

all_points_progression %>%
  ggplot() + geom_line(aes(x=game, y=value, color=model)) + facet_wrap(~variable)

for (t in sort(colnames(pts_prog))) {
  y_real = pts_prog[[t]]
  y_v1 = sim_pts_prog_1[[t]]
  y_v2 = sim_pts_prog_2[[t]]
  
  plot(x,y_real,xlim = c(0,34),ylim = c(0,60),type = "l", col="black", main=t, xlab="Games", ylab="Points")
  lines(x, y_v1, pch=18, col="red", type="l", lwd=2)
  lines(x, y_v2, pch=18, col="green", type="l", lwd=2)
  legend(1, 50, legend=c("Real values", "Model v1", "Model v2"),
         col=c("black", "red", "green"), lty=1:1, lwd=2:2)
}

png("plots/1991_attack.png", width=1200, height=700)
best_attack_plot
dev.off()

png("plots/1991_defense.png", width=1200, height=700)
worst_defense_plot
dev.off()

png("plots/1991_home_goal_dist.png", width=1200, height=700)
home_goal_dist
dev.off()

png("plots/1991_away_goal_dist.png", width=1200, height=700)
away_goal_dist
dev.off()

png("plots/1991_full_time_hm.png", width=1200, height=700)
full_time_results_hm
dev.off()

rm(list=ls())
