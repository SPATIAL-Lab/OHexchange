model {
  # Apply to each sample individually
  for(i in 1:nsamples) {
    # Forward model for each measurement
    d18O_p[i] = (1 - f_ex) * d18O_t[tooth[i]] + 
      f_ex * a_ex * ((d18O_a[i] + d18O_w[i])/2 + (a_ex - 1) * 1000)
    
    # Likelihood for each measurement
    d18O_m[i, 1] ~ dnorm(d18O_p[i], 1 / pow(d18O_m[i, 2], 2))
  }
  
  # Apply to all samples of a given tooth
  for(i in 1:nteeth){
    d18O_t[i] ~ dnorm(d18O_ut[i, 1], 1 / pow(d18O_ut[i, 2], 2))
  }
  
  # Apply to all samples
  # Priors (lower = fractionation factor, upper = fraction measured 18O deriving from water)
  f_ex ~ dunif(0, 0.3)
  a_ex ~ dunif(0.990, 1.2)
}

