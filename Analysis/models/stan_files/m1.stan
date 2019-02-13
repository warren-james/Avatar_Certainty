data {
   int<lower = N> N;       // No. observations
   vector[N] Abs_Norm_pos; // Norm_pos
   vector[N] Norm_Delta;   // Delta          
}
parameters {
   real a;                // Intercept
   real b_delta;          // Delta slope
   real<lower = 0> sigma;
}
model{

}
