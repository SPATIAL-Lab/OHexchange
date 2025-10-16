jags_code <- "
model {
  # per-sample predictions + likelihood
  for(i in 1:nsamples) {
    # predicted d18O is weighted sum of tooth / acid / water contributions
    d18O_p[i] <- f[1] * d18O_t[tooth[i]] +
      f[2] * a_ex_acid * (d18O_a[i] + d18O_w[i] + (a_ex_acid - 1) * 1000) +
      f[3] * a_ex_water * (d18O_a[i] + d18O_w[i] + (a_ex_water - 1) * 1000)
    
    # likelihood: observed value (col 1) with known measurement sd (col 2)
    d18O_m[i, 1] ~ dnorm(d18O_p[i], 1 / pow(d18O_m[i, 2], 2))
  }
  
  # tooth-level priors (one prior per tooth)
  for(j in 1:nteeth) {
    d18O_t[j] ~ dnorm(d18O_ut[j, 1], 1 / pow(d18O_ut[j, 2], 2))
  }
  
  # Dirichlet prior on mixture weights (tooth, acid, water)
  # Example: uninformative = c(1,1,1) ; if you expect water to dominate use e.g. c(1,1,8)
  f[1:3] ~ ddirch(c(1, 1, 1))
  
  # priors for exchange/fractionation factors
  a_ex_acid  ~ dunif(0.95, 1.3)
  a_ex_water ~ dunif(0.95, 1.3)
}
"