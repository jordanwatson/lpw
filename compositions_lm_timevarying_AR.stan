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
  vector[N] sd_pro; 
  vector[N-1] sd_dev;
}

transformed parameters {
  matrix[N, A] exp_p_transform; 
  simplex[A] p[N];  // predicted proportions  
  real exp_p_tot[N]; // normalizing constant  
  matrix[S, A-1] p_B0; // estimated slopes by age and stock, hierarchical

  
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

  sd_pro[1] ~ student_t(3, 0, 3); // initialize sd_pro at time t=1
  hypersd ~ student_t(3, 0, 3); 
  b ~ normal(0, 1);
  
   // process deviations. We have added a time varying standard deviation to pro_dev (independent of age)
 for(i in 1:N) {
    for(a in 1:(A-1)) {
      pro_dev[i,a] ~ normal(0, sd_pro[i]);
    }
  }

  // sd_pro varies over time with a deviation of sd_dev from the previous time step.
  // I think that sd_dev should maybe be [i] instead of [i-1]. But maybe it doesn't matter.
  for(i in 2:N) {
    sd_pro[i] = sd_pro[i-1] + sd_dev[i-1];
  }
  
  // define the variability of the deviation for sd_pro
  for(i in 2:(N)) {
      sd_dev[i] ~ lognormal(sd_dev[i-1]+(hypersd*hypersd/2), hypersd); 
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
