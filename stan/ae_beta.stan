data{
  int N1;
  int N2;
  vector <lower = 0, upper = 1>[N1] x1;
  vector <lower = 0, upper = 1>[N2] x2;
}


parameters{
  
  real<lower = 0, upper = 1> mu1;
  
  real<lower=0,upper=1> phi1;
  real<lower=0,upper=1> phi2;
  
  real<lower = 0, upper = 1> a;


}

transformed parameters{

  real<lower=0,upper=1> mu2;    // transformed linear predictor for mean of beta distribution


  real<lower=0> A1;             // parameter for beta distn
  real<lower=0> B1;             // parameter for beta distn
  real<lower=0> A2;             // parameter for beta distn
  real<lower=0> B2;             // parameter for beta distn

  mu2 = (1 - a) * mu1;

  A1 = mu1 * phi1;
  B1 = (1.0 - mu1) * phi1;
  A2 = mu2 * phi2;
  B2 = (1.0 - mu2) * phi2;
}

model{
  
  // priors
 // phi1 ~ beta(1, 1); // uniform on phi, could drop
  //phi2 ~ beta(1, 1); // uniform on phi, could drop

  x1 ~ beta(A1, B1);
  x2 ~ beta(A2, B2);

}

generated quantities{

}

