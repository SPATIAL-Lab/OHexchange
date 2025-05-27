model {
  #### Forward model for isotope exchange between tooth enamel and water
  ## Data include d18O_w, d18O_ut (mean and sd), d18O_m (mean and sd)
  
  # Prior parameter estimates
  ## True d18O of tooth enamel carbonate
  d18O_t ~ dnorm(d18O_ut[1], 1 / d18O_ut[2] ^ 2)
  ## Fractionation factor between water and enamel hydroxyl
  a_ex ~ dunif(0.990, 1.01)
  ## Fraction of measured d18O that derives from water
  f_ex ~ dunif(0, 0.3)
  
  # Model
  d18O_p = (1 - f_ex) * d18O_t + f_ex * a_ex * (d18O_w + (a_ex - 1) * 1000)
  
  # Likelihood
  d18O_m[1] ~ dnorm(d18O_p, 1 / d18O_m[2] ^ 2)
}
