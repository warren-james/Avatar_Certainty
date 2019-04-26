data {
   int<lower = N> N;       // No. Observations
   vector[N] Abs_Norm_pos; // Dependant Variable
   int<lower = 1> K;       // No. Predictors
   matrix[N,K] X;          // Predictors     
}
parameters {
   vector[K] beta;    // coefs for mu
   vector[K] gamma;   // coefs for phi
}
transformed parameters {
   vector<lower = 0, upper = 1>[N] mu; // transformed linear predictor for mean
   vector<lower = 0>[N] phi;           // transformed linear predictor for precision
   vector<lower = 0>[N] A;             // parameter for beta dist 
   vector<lower = 0>[N] B;             // parameter for beta dist
 
   for(i in 1:N) {
       mu[i] = inv_logit(X[i,] * beta);
       phi[i] = exp(X[i,] * gamma);
   }
   A = mu .* phi;
   B = (1.0 - mu) .* phi;
}
model {
   // priors
   // need to work on this part...
   // 1 is the intercept  
   beta[1] ~ normal(0, 0.5); 
   beta[2] ~ normal(0, 0.5);



   gamma[1] ~ normal(0, 1);

   gamma[2] ~ normal(0, 1);

   // likelihood

   y ~ beta(A, B);
}
