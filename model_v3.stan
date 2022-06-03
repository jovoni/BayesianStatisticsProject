data {
  int<lower=2> N;
  int<lower=2> T;
  
  int hometeam[N];
  int awayteam[N];
  
  int y1[N];
  int y2[N];
}

parameters {
  vector[T] home;
  vector[T] att_star;
  vector[T] def_star;
  
  real mu_att;
  real mu_def;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
}

transformed parameters {
  vector[T] att;
  vector[T] def;
  
  att = att_star - mean(att_star);
  def = def_star - mean(def_star);
}

model {
  for (t in 1:T) {
    home[t] ~ normal(0, 10);  
  }
  
  mu_att ~ normal(0, 10);
  mu_def ~ normal(0, 10);
  
  sigma_att ~ inv_gamma(1, 1);
  sigma_def ~ inv_gamma(1, 1);
  
  for (t in 1:T) {
    att_star[t] ~ normal(mu_att, sigma_att);
    def_star[t] ~ normal(mu_def, sigma_def);
  }
  
  for (n in 1:N) {
    y1[n] ~ poisson_log(home[hometeam[n]] + att[hometeam[n]] + def[awayteam[n]]);
    y2[n] ~ poisson_log(att[awayteam[n]] + def[hometeam[n]]);
  }
}

generated quantities {
  int<lower=0> y1_new[N];
  int<lower=0> y2_new[N];
  
  vector[N] log_lik_1;
  vector[N] log_lik_2;
  
  for (n in 1:N) {
    log_lik_1[n] = poisson_log_lpmf(y1[n] | home[hometeam[n]] + att[hometeam[n]] + def[awayteam[n]]);
    y1_new[n] = poisson_log_rng(home[hometeam[n]] + att[hometeam[n]] + def[awayteam[n]]);
    
    log_lik_2[n] = poisson_log_lpmf(y2[n] | att[awayteam[n]] + def[hometeam[n]]);
    y2_new[n] = poisson_log_rng(att[awayteam[n]] + def[hometeam[n]]);
  }
}