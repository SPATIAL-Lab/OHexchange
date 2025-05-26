library(R2jags)

# Read in datasets
treat = read.csv("data/BRT1-T7_treatments.csv")
result = read.csv("data/BRT1-T7results.csv")

# Untreated (aka true) enamel d18O
d18O_ut = result$d18O[result$Tooth.ID == "BR1" & result$Treatment.Number == 0]
SD_ut = result$StDev[result$Tooth.ID == "BR1" & result$Treatment.Number == 0]
d18O_ut = c(d18O_ut, SD_ut)

# Measured enamel d18O
d18O_m = result$d18O[result$Tooth.ID == "BR1" & result$Treatment.Number == 5]
SD_m = result$StDev[result$Tooth.ID == "BR1" & result$Treatment.Number == 5]
d18O_m = c(d18O_m, SD_ut)

# Treatment water d18O
d18O_w = treat$Rinse.d18O[treat$Treatment.Number == 5]

# Bundle for JAGS
d = list(d18O_ut = d18O_ut, d18O_m = d18O_m, d18O_w = d18O_w)

# Parameters to save
p = c("d18O_t", "a_ex", "f_ex", "d18O_p")

# Run the JAGS analysis
post = jags(d, NULL, p, "code/singleSampleJAGS.R")

# View the summary results and traceplots
View(post$BUGSoutput$summary)
traceplot(post)

# Look at some of the posterior distributions
plot(density(post$BUGSoutput$sims.list$f_ex))
plot(density(post$BUGSoutput$sims.list$a_ex))

# Look at bivariate parameter space
plot(post$BUGSoutput$sims.list$a_ex, post$BUGSoutput$sims.list$f_ex)
