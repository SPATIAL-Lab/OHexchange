model {
  for (i in 1:I) {
    d18O_t[i] ~ dnorm(d18O_ut[i], 1 / pow(sd_ut[i], 2))
  }
  
  for (n in 1:N) {
    # Forward model for each measurement
    d18O_p[n] <- (1 - f_ex[n]) * d18O_t[sample_id[n]] +
      f_ex[n] * a_ex[n] * (d18O_w[treat_id[n]] + (a_ex[n] - 1) * 1000)
    
    # Likelihood for each measurement
    d18O_m[n] ~ dnorm(d18O_p[n], 1 / pow(sd_m[n], 2))
    
    # Priors (lower = fractionation factor, upper = fraction measured 18O deriving from water)
    f_ex[n] ~ dunif(0, 0.3)
    a_ex[n] ~ dunif(0.990, 1.01)
  }
}
