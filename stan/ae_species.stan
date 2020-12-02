data{
  int N;
  int M;
  vector <lower = 0>[N] c1_p;
  vector <lower = 0>[N] n1_p;
  vector <lower = 0>[N] p1_p;
  vector <lower = 0>[M] c2_p;
  vector <lower = 0>[M] n2_p;
  vector <lower = 0>[M] p2_p;
  int S; // # of species
  int<upper = S> s1[N]; // which species
  int<upper = S> s2[M]; // which species for ae2
}

transformed data{
  vector <lower = 0>[N] c1_sd;
  vector <lower = 0>[N] n1_sd;
  vector <lower = 0>[N] p1_sd;
  vector <lower = 0>[M] c2_sd;
  vector <lower = 0>[M] n2_sd;
  vector <lower = 0>[M] p2_sd;
  c1_sd = 0.05 * c1_p;
  n1_sd = 0.1 * n1_p;
  p1_sd = 0.08 * p1_p;
  c2_sd = 0.05 * c2_p;
  n2_sd = 0.1 * n2_p;
  p2_sd = 0.08 * p2_p;
}

parameters{
  vector <lower = 0>[N] c1;
  vector <lower = 0>[N] n1;
  vector <lower = 0>[N] p1;
  vector <lower = 0>[M] c2;
  vector <lower = 0>[M] n2;
  vector <lower = 0>[M] p2;
  
  vector<lower = 0, upper = 100>[S] mu_c1;
  vector<lower = 0, upper = 100>[S] mu_n1;
  vector<lower = 0, upper = 100>[S] mu_p1; 

  vector<lower = 0, upper = 100>[S] sd_c1;
  vector<lower = 0, upper = 100>[S] sd_n1;
  vector<lower = 0, upper = 100>[S] sd_p1; 
 
  vector<lower = 0, upper = 100>[S] sd_c2;
  vector<lower = 0, upper = 100>[S] sd_n2;
  vector<lower = 0, upper = 100>[S] sd_p2; 

  vector<lower = 0, upper = 1>[S] ac;
  vector<lower = 0, upper = 1>[S] an;
  vector<lower = 0, upper = 1>[S] ap;
}

transformed parameters{

}

model{
  vector[S] mu_c2;
  vector[S] mu_n2;
  vector[S] mu_p2; 

  target += student_t_lpdf(c1_sd | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(n1_sd | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(p1_sd | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(c2_sd | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(n2_sd | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(p2_sd | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  

  
  //c1_p ~ normal(c1, c1_sd);
  //n1_p ~ normal(n1, n1_sd);
  //p1_p ~ normal(p1, p1_sd);
  //c2_p ~ normal(c2, c2_sd);
  //n2_p ~ normal(n2, n2_sd);
  //p2_p ~ normal(p2, p2_sd);
  
  ac ~ normal(0.5, 0.2);
  an ~ normal(0.5, 0.2);
  ap ~ normal(0.5, 0.2);
  
  mu_c1 ~ normal(20, 10);
  mu_n1 ~ normal(5, 5);
  mu_p1 ~ normal(2,2);
  
  for (j in 1:S){
    mu_c2[j] = (1 - ac[j]) * mu_c1[j];
    mu_n2[j] = (1 - an[j]) * mu_n1[j];
    mu_p2[j] = (1 - ap[j]) * mu_p1[j];
  }

  for (n in 1:N){
      c1_p[n] ~ normal(mu_c1[s1[n]], sd_c1[s1[n]]);
      n1_p[n] ~ normal(mu_c1[s1[n]], sd_c1[s1[n]]);
      p1_p[n] ~ normal(mu_c1[s1[n]], sd_c1[s1[n]]);
  }
  for (m in 1:M){
      c2_p[m] ~ normal(mu_c2[s2[m]], sd_c2[s2[m]]);
      n2_p[m] ~ normal(mu_c2[s2[m]], sd_n2[s2[m]]);
      p2_p[m] ~ normal(mu_c2[s2[m]], sd_p2[s2[m]]);
  }
  

  

}

