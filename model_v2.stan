data {
  int<lower=2> N;
  int<lower=2> T;
  
  int hometeam[N];
  int awayteam[N];
  
  int y1[N];
  int y2[N];
  
  vector[3] dir_alpha_att[T];
  vector[3] dir_alpha_def[T];
}

parameters {
  real home;
  
  simplex[3] pi_attack[T];
  simplex[3] pi_defense[T];
  
  real<lower=-3, upper=0> mu_att_bottom;
  real mu_att_middle;
  real<lower=0, upper=3> mu_att_top;
  
  real<lower=0, upper=3> mu_def_bottom;
  real mu_def_middle;
  real<lower=-3, upper=0> mu_def_top;
  
  vector[T] att_star;
  vector[T] def_star;
  
  //real<lower=0> tau_att[3];
  //real<lower=0> tau_def[3];
  real<lower=0> sigma_att[3];
  real<lower=0> sigma_def[3];
}

transformed parameters {
  //real<lower=0> sigma_att[3];
  //real<lower=0> sigma_def[3];
  
  vector[T] att;
  vector[T] def;
  
  //for (i in 1:3) {
  //  sigma_att[i] = inv(tau_att[i]);
  //  sigma_def[i] = inv(tau_def[i]);
  //}
  
  att = att_star - mean(att_star);
  def = def_star - mean(def_star);
}

model {
  vector[3] mix_attack;
  vector[3] mix_defense;
  
  home ~ normal(0, 100);
  
  // bottom teams
  //tau_att[1] ~ gamma(0.1, 0.1);
  //tau_def[1] ~ gamma(0.1, 0.1);
  sigma_att[1] ~ inv_gamma(1,1);
  sigma_def[1] ~ inv_gamma(1,1);
  mu_att_bottom ~ normal(0, 1);
  mu_def_bottom ~ normal(0, 1);
  
  // middle teams
  //tau_att[2] ~ gamma(0.1, 0.1);
  //tau_def[2] ~ gamma(0.1, 0.1);
  sigma_att[2] ~ inv_gamma(1,1);
  sigma_def[2] ~ inv_gamma(1,1);
  mu_att_middle ~ normal(0, 1);
  mu_def_middle ~ normal(0, 1);
  
  // top teams
  //tau_att[3] ~ gamma(0.1, 0.1);
  //tau_def[3] ~ gamma(0.1, 0.1);
  sigma_att[3] ~ inv_gamma(1,1);
  sigma_def[3] ~ inv_gamma(1,1);
  mu_att_top ~ normal(0, 1);
  mu_def_top ~ normal(0, 1);
  
  
  for (t in 1:T) {
    pi_attack[t,] ~ dirichlet(dir_alpha_att[t]);
    pi_defense[t,] ~ dirichlet(dir_alpha_def[t]);
    
    mix_attack[1] = log(pi_attack[t,1]) + student_t_lpdf(att[t] | 4, mu_att_bottom, sigma_att[1]); 
    mix_defense[1] = log(pi_defense[t,1]) + student_t_lpdf(def[t] | 4, mu_def_bottom, sigma_def[1]);
    
    mix_attack[2] = log(pi_attack[t,2]) + student_t_lpdf(att[t] | 4, mu_att_middle, sigma_att[2]); 
    mix_defense[2] = log(pi_defense[t,2]) + student_t_lpdf(def[t] | 4, mu_def_middle, sigma_def[2]);
    
    mix_attack[3] = log(pi_attack[t,3]) + student_t_lpdf(att[t] | 4, mu_att_top, sigma_att[3]); 
    mix_defense[3] = log(pi_defense[t,3]) + student_t_lpdf(def[t] | 4, mu_def_top, sigma_def[3]);
    
    target += log_sum_exp(mix_attack);
    target += log_sum_exp(mix_defense);
  }
  
  for (n in 1:N) {
    y1[n] ~ poisson_log(home + att[hometeam[n]] + def[awayteam[n]]);
    y2[n] ~ poisson_log(att[awayteam[n]] + def[hometeam[n]]);
  }
}

generated quantities {
  int<lower=0> y1_new[N];
  int<lower=0> y2_new[N];
  
  vector[N] log_lik_1;
  vector[N] log_lik_2;
  
  for (n in 1:N) {
    log_lik_1[n] = poisson_log_lpmf(y1[n] | home + att[hometeam[n]] + def[awayteam[n]]);
    y1_new[n] = poisson_log_rng(home + att[hometeam[n]] + def[awayteam[n]]);
    
    log_lik_2[n] = poisson_log_lpmf(y2[n] | att[awayteam[n]] + def[hometeam[n]]);
    y2_new[n] = poisson_log_rng(att[awayteam[n]] + def[hometeam[n]]);
  }
}
