d18O_m_raw <- read.csv("data/BRT1-T7results.csv", stringsAsFactors = FALSE)
treated_idx <- which(d18O_raw$Treatment.Number  != 0)
# Keep only the relevant columns
d18O_m <- as.matrix(d18O_m_raw[, c("d18O", "StDev")])
d18O_m_treated <- d18O_m[treated_idx, ]  # keep only treated rows

d18O_ut_raw <- as.matrix(data.frame(lapply(d18O_ut, as.numeric)))
# Keep only the relevant columns
d18O_ut <- as.matrix(d18O_ut_raw[, c("d18O", "StDev")])


data_list <- list(
  nsamples = nrow(d18O_m),
  nteeth = length(unique(tooth)),
  tooth = tooth,            # vector of tooth indices for each sample (1..nteeth)
  d18O_m = d18O_m_treated,          # matrix nsamples x 2: col1 = observed, col2 = sd
  d18O_a = d18O_a,          # acid composition for each sample (or scalar)
  d18O_w = d18O_w,          # water composition for each sample (or scalar)
  d18O_ut = d18O_ut         # nteeth x 2 matrix: prior mean, prior sd for each tooth
)

inits <- function() {
  list(
    f = c(0.8, 0.1, 0.1),         # a reasonable starting mixture
    a_ex_acid = 1.02,
    a_ex_water = 1.02,
    d18O_t = rnorm(data_list$nteeth, 0, 1)  # random inits for each tooth
  )
}

parameters <- c("f", "a_ex_acid", "a_ex_water", "d18O_t", "d18O_p")

library(rjags)
jags_model <- jags.model(textConnection(jags_code), data = data_list, inits = inits, n.chains = 3)
update(jags_model, 2000)

samps <- coda.samples(jags_model, variable.names = parameters, n.iter = 10000)

df <- as.data.frame(as.matrix(samps))

# Compute posterior mean predicted value for each sample
d18O_p_means <- colMeans(df[, grep("^d18O_p", names(df))])


# Plot measured vs predicted
plot(d18O_m_treated[,1], d18O_p_means,
     xlab = "Observed δ18O", ylab = "Predicted δ18O",
     main = "Model fit: observed vs. predicted")
abline(0, 1, col = "red", lty = 2)
