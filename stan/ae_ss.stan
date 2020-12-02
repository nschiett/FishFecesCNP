data{
  int N;
  int M;
  vector <lower = 0>[N] c1;
  vector <lower = 0>[N] n1;
  vector <lower = 0>[N] p1;
  vector <lower = 0>[M] c2;
  vector <lower = 0>[M] n2;
  vector <lower = 0>[M] p2;
}


parameters{
  
  real<lower = 0> nu1;
  real<lower = 0> nu2;
  real<lower = 0> nu3;
  
  real<lower = 0, upper = 100> mu_c1;
  real<lower = 0, upper = 100> mu_n1;
  real<lower = 0, upper = 100> mu_p1; 

  real<lower = 0, upper = 100> sd_c1;
  real<lower = 0, upper = 100> sd_n1;
  real<lower = 0, upper = 100> sd_p1;
  
  real<lower = 0, upper = mu_c1> mu_c2;
  real<lower = 0, upper = mu_n1> mu_n2;
  real<lower = 0, upper = mu_p1> mu_p2; 
 
  real<lower = 0, upper = 100> sd_c2;
  real<lower = 0, upper = 100> sd_n2;
  real<lower = 0, upper = 100> sd_p2; 


}

transformed parameters{
  real<lower = 0, upper = 1> ac;
  real<lower = 0, upper = 1> an;
  real<lower = 0, upper = 1> ap;
  
  ac = 1 - (mu_c2/mu_c1);
  an = 1 - (mu_n2/mu_n1);
  ap = 1 - (mu_p2/mu_p1);
  
}

model{


  target += student_t_lpdf(sd_c1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(sd_n1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(sd_p1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(sd_c2 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(sd_n2 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(sd_p2 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  
  nu1 ~ gamma(2,0.1);
  nu2 ~ gamma(2,0.1);
  nu3 ~ gamma(2,0.1);
  
  c1 ~ student_t(nu1, mu_c1, sd_c1);
  n1 ~ student_t(nu1, mu_n1, sd_n1);
  p1 ~ student_t(nu2, mu_p1, sd_p1);
  c2 ~ student_t(nu2, mu_c2, sd_c2);
  n2 ~ student_t(nu3, mu_n2, sd_n2);
  p2 ~ student_t(nu3, mu_p2, sd_p2);
  
  

}

