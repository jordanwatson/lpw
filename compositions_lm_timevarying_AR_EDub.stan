data {
  int<lower=1> A; // number of ages
  int<lower=1> S; // number of stocks
  int<lower=1> N; // number of data points (rows)
  vector<lower=0>[A] alpha; // prior  
  int<lower=1> year_id[N]; // integer id of years, starts at 1
  int<lower=0> ages[N, A];
}

parameters {
  real b[A-1];
  simplex[A] p_mean[S];  // predicted proportions at time = 1
  matrix[N, A-1] pro_dev; // proportion deviation
  real<lower=0> hypersd; // deviation of AR term
  vector[N-1] sd_dev;
  real<lower=0> sd0;
}

transformed parameters {
  matrix[N, A] exp_p_transform; 
  simplex[A] p[N];  // predicted proportions  
  real exp_p_tot[N]; // normalizing constant  
  matrix[S, A-1] p_B0; // estimated slopes by age and stock, hierarchical
  vector<lower=0>[N] sd_pro; 
  
  // sd_pro varies over time with a deviation of sd_dev from the previous time step.
  sd_pro[1] = sd0; // initialize sd_pro at time t=1
  for(i in 2:N) {
    sd_pro[i] = exp(log(sd_pro[i-1]) + sd_dev[i-1]);
  }
  
  // calculate p_B0 as derived parameter
  for(s in 1:S) {
    for(a in 1:(A-1)) {
      p_B0[s, a] = log(p_mean[s,a]);
    }
  }
  
  for(i in 1:N) {
    for(a in 1:(A-1)) {
      exp_p_transform[i,a] = exp(p_B0[1,a] + b[a]*(i-1) + pro_dev[i,a]);
    }  	
    exp_p_transform[i,A] = 1;  
    exp_p_tot[i] = sum(exp_p_transform[i,]);
    
    // calculate predicted proportions, normalized by sum
    p[i] = to_vector(exp_p_transform[i] / exp_p_tot[i]);
  }  
}

model {
  sd0 ~ student_t(3, 0, 3);
  hypersd ~ student_t(3, 0, 3); 
  b ~ normal(0, 1);
  
   // process deviations. We have added a time varying standard deviation to pro_dev (independent of age)
 for(i in 1:N) {
    for(a in 1:(A-1)) {
      pro_dev[i,a] ~ normal(0, sd_pro[i]);
    }
  }
  
  // define the variability of the deviation for sd_pro
  sd_dev[1] ~ normal(0, 3);
  for(i in 2:(N-1)) {
  //    sd_dev[i] ~ lognormal(sd_dev[i-1]+(hypersd*hypersd/2), hypersd); 
  sd_dev[i] ~ normal(sd_dev[i-1], hypersd); 
  }


  // intercept in first year, by age and stock
  for(s in 1:S) {
    p_mean[s] ~ dirichlet(alpha); // estimated proportions in year 1, independent by stock
  }  
  
  // likelihood
  for(i in 1:N) {
    ages[i] ~ multinomial(p[i]);
  }
  
}
