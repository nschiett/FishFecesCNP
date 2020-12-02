data{
  int N1;
  int N2;
  vector <lower = 0, upper = 100>[N1] x1;
  vector <lower = 0, upper = 100>[N2] x2;
}


parameters{
  
  real<lower = 0, upper = 100> mu1;
  
  real<lower=0> sigma1;
  real<lower=0> sigma2;
  
  real<lower = 0, upper = 1> a;


}

transformed parameters{

  real<lower=0,upper=100> mu2;  
  
  mu2 = (1 - a) * mu1;

}

model{

  x1 ~ normal(mu1, sigma1);
  x2 ~ normal(mu2, sigma2);

}

generated quantities{

}

