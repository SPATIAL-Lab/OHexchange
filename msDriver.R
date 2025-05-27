# Define sample IDs
tooth_ids <- unique(result$Tooth.ID)
N <- length(tooth_ids)

# Create placeholders
d18O_all_ut <- matrix(NA, nrow = N, ncol = 2)
d18O_ALL_m  <- matrix(NA, nrow = N, ncol = 2)
d18O_all_w  <- numeric(N)

# Populate matrices
for (i in 1:N) {
  id <- tooth_ids[i]
  
  # Get untreated enamel values
  d18O_all_ut[i, 1] <- result$d18O[result$Tooth.ID == id & result$Treatment.Number == 0]
  d18O_all_ut[i, 2] <- result$StDev[result$Tooth.ID == id & result$Treatment.Number == 0]
  
  # Get treated enamel values (e.g., treatment 5)
  d18O_all_m[i, 1] <- result$d18O[result$Tooth.ID == id & result$Treatment.Number == 5]
  d18O_all_m[i, 2] <- result$StDev[result$Tooth.ID == id & result$Treatment.Number == 5]
  
  # Get treatment water δ18O (assuming same water for all)
  d18O_all_w[i] <- treat$Rinse.d18O[treat$Treatment.Number == 5]
}

# Running the model
library(R2jags)

params <- c("d18O_t", "a_ex", "f_ex", "d18O_p")  # Parameters to track

fit <- jags(
  data = data_jags,
  inits = NULL,
  parameters.to.save = params,
  model.file = "C:/Users/benri/OneDrive/Documents/OHexchange/multisampleJAGS.R",
  n.chains = 3,
  n.iter = 5000,
  n.burnin = 1000
)

# Displaying it
# Posterior distributions for parameter i
plot(density(fit$BUGSoutput$sims.list$f_ex[,1]))  # f_ex for sample 1

#View and traceplots
View(fit$BUGSoutput$summary)
traceplot(fit)
