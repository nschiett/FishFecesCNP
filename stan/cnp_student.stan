data{
  int N1;
  int N2;
  vector[3] x1[N1]; 
  vector[3] x2[N2]; 
  

}


parameters{
  
  real<lower = 0, upper = 100> Dc;
  real<lower = 0, upper = 100> Wc;
  real<lower = 0, upper = 100> Dn;
  real<lower = 0, upper = 100> Wn;
  real<lower = 0, upper = 100> Dp;
  real<lower = 0, upper = 100> Wp;
  
  cov_matrix[3] Sigma1;
  cov_matrix[3] Sigma2;
  
  real<lower=0> nu; //degrees of freedom
  

}

transformed parameters{
  vector[3] mu1;
  vector[3] mu2;


  mu1[1] = Dc;
  mu1[2] = Dn;
  mu1[3] = Dp;
  mu2[1] = Wc;
  mu2[2] = Wn;
  mu2[3] = Wp;


}

model{

  nu ~ gamma(2, 0.1);

  x1 ~ multi_student_t(nu, mu1, Sigma1); 
  x2 ~ multi_student_t(nu, mu2, Sigma2); 


}

generated quantities{
  real Dnp = Dn/Dp;
  real Dcn = Dc/Dn;
  real Dcp = Dc/Dp;
  real Wnp = Wn/Wp;
  real Wcn = Wc/Wn;
  real Wcp = Wc/Wp;
  
  real Acn = (Dc*Wn)/(Wc*Dn);
  real Acp = (Dc*Wp)/(Wc*Dp);

}

