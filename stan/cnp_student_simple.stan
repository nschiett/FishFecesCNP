data{
  int N1;
  int N2;
  real c1[N1]; 
  real c2[N2]; 
  real n1[N1]; 
  real n2[N2]; 
  real p1[N1]; 
  real p2[N2]; 
  
  real ar; // ash ratio
}


parameters{
  
  real<lower = 0, upper = 60> Dc;
  real<lower = 0, upper = 60> Wc;
  real<lower = 0, upper = 20> Dn;
  real<lower = 0, upper = 20> Wn;
  real<lower = 0, upper = 10> Dp;
  real<lower = 0, upper = 10> Wp;

  real<lower=0> nuc; //degrees of freedom
  real<lower=0> nun; //degrees of freedom
  real<lower=0> nup; //degrees of freedom
  
  real<lower=0> sigma_c1;
  real<lower=0> sigma_c2;
  real<lower=0> sigma_n1;
  real<lower=0> sigma_n2;
  real<lower=0> sigma_p1;
  real<lower=0> sigma_p2;
  

}

transformed parameters{


}

model{

  nuc ~ gamma(2, 0.1);
  nun ~ gamma(2, 0.1);
  nup ~ gamma(2, 0.1);
  
  Dc ~ normal(30, 30);
  Wc ~ normal(30, 30);
  Dn ~ normal(5, 5);
  Wn ~ normal(5, 5);
  Dp ~ normal(1, 1);
  Wp ~ normal(1, 1);

  
  sigma_c1 ~ cauchy(0, 5);
  sigma_c2 ~ cauchy(0, 5);
  sigma_n1 ~ cauchy(0, 1);
  sigma_n2 ~ cauchy(0, 1);
  sigma_p1 ~ cauchy(0, 0.5);
  sigma_p2 ~ cauchy(0, 0.5);

  c1 ~ student_t(nuc, Dc, sigma_c1); 
  c2 ~ student_t(nuc, Wc, sigma_c2); 

  n1 ~ student_t(nun, Dn, sigma_n1); 
  n2 ~ student_t(nun, Wn, sigma_n2); 
  
  p1 ~ student_t(nup, Dp, sigma_p1); 
  p2 ~ student_t(nup, Wp, sigma_p2); 

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
  
  real Rc = Wc/Dc;
  real Rn = Wn/Dn;
  real Rp = Wp/Dp;
  
  real ac = 1 - (ar*(Wc/Dc));
  real an = 1 - (ar*(Wn/Dn));
  real ap = 1 - (ar*(Wp/Dp));


}

