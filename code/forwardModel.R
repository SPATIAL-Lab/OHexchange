#### Forward model for isotope exchange between tooth enamel and water
#### For experimentation only

# Parameters
## Known
### Measured value of treatment water
d18O_w = -16
### Measured value of enamel d18O
d18O_m = -10
  
## Unknown
### True d18O of tooth enamel carbonate
d18O_t = -11
### Fractionation factor between water and enamel hydroxyl
a_ex = 1.010
### Fraction of measured d18O that derives from water
f_ex = seq(0, 1, by = 0.01)

# Model equation
d18O_p = (1 - f_ex) * d18O_t + f_ex * a_ex * (d18O_w + (a_ex - 1) * 1000)

# Plot predicted enamel vs exchange fraction
plot(f_ex, d18O_p, xlab = "Exchange fraction", ylab = "Predicted enamel d18O")
abline(h = d18O_m)
