library(R2jags)

# Read in datasets
treat = read.csv("data/BRT1-T7_treatments.csv")
result = read.csv("data/BRT1-T7results.csv")

# Untreated (aka true) enamel d18O
d18O_ut = result[result$Treatment.Number == 0, ]

# Measured enamel d18O
d18O_m = result[result$Treatment.Number != 0, ]

# Tooth index linking d18O_m to d18O_ut
tooth = match(d18O_m$Tooth.ID, d18O_ut$Tooth.ID)

# Treatment water d18O
d18O_w = treat$Rinse.d18O[match(d18O_m$Treatment.Number, treat$Treatment.Number)]

# Bundle for JAGS
d = list(d18O_ut = d18O_ut[, 3:4], d18O_m = d18O_m[, 3:4], d18O_w = d18O_w,
         nteeth = nrow(d18O_ut), nsamples = nrow(d18O_m), tooth = tooth)

# Parameters to save
p = c("d18O_t", "a_ex", "f_ex", "d18O_p")

# Run the JAGS analysis
post = jags(d, NULL, p, "code/multisampleJAGS.R", n.iter = 5000,
            n.burnin = 1000)

View(post$BUGSoutput$summary)
traceplot(post)
dev.off()

# Compare predicted with measured
plot(d18O_m$d18O, post$BUGSoutput$median$d18O_p, pch = 21, 
     bg = d18O_m$Treatment.Number, xlab = "Measured d18O", ylab = "Predicted d18O")
abline(0, 1)
legend("bottomright", legend = treat$Rinse.d18O, pch = 21, 
       pt.bg = 1:7, bty = "n")

#Same plot as above, colored by tooth
result$ID <- as.factor(result$Tooth.ID)
colors <- setNames(rainbow(length(levels(result$ID))), 
                   levels(result$ID))

plot(d18O_m$d18O, post$BUGSoutput$median$d18O_p, pch = 21, 
     bg = colors[ID], xlab = "Measured d18O", ylab = "Predicted d18O")
abline(0, 1)
legend("bottomright", legend = unique(result$Tooth.ID), pch = 21, 
       pt.bg = 1:7, bty = "n", cex = 0.8, ncol = 2)

# Combining the above two (colors for treatment, shapes for tooth)
# This one didn't really work out, it's difficult to read and there doesn't seem to be enough shapes that play well with the fill colors to make it work.
shapes <- as.factor(d18O_m$Treatment.Number)
pch_values <- c(19, 20, 21, 22, 23, 24, 25)[as.numeric(shapes)]

plot(d18O_m$d18O, post$BUGSoutput$median$d18O_p, pch = pch_values, 
     bg = colors[ID], xlab = "Measured d18O", ylab = "Predicted d18O")
abline(0, 1)
legend("bottomright", legend = unique(result$Tooth.ID), pch = 21, 
       pt.bg = 1:7, cex = 0.8, ncol = 2, title = "Tooth ID", bty = "n")
legend("topleft", legend = treat$Rinse.d18O, pch = c(19, 20, 21, 22, 23, 24, 25),
       pt.bg = "gray",col = "black", cex = 0.8, 
       title = "Rinse d18O", bty = "n")

# Compare true with untreated
plot(d18O_ut$d18O, post$BUGSoutput$median$d18O_t, pch = 21, 
     bg = "white")
abline(0, 1)

#________________________________________________________________________
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
