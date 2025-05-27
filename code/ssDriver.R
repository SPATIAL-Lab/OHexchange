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
d18O_m = c(d18O_m, SD_m)

# Treatment water d18O
d18O_w = treat$Rinse.d18O[treat$Treatment.Number == 5]

# Bundle for JAGS
d = list(d18O_ut = d18O_ut, d18O_m = d18O_m, d18O_w = d18O_w)

#Data frame with all UT data
df_UT = 
data.frame(d18O_UT = result$d18O[result$Treatment.Number == "0"], 
           SD_UT = result$StDev[result$Treatment.Number == "0"])

#Data frame with all measured data
df_m = 
data.frame(d18O_T1 = result$d18O[result$Treatment.Number == "1"], 
           SD_T1 = result$StDev[result$Treatment.Number == "1"], 
           d18O_T2 = result$d18O[result$Treatment.Number == "2"], 
           SD_T2 = result$StDev[result$Treatment.Number == "2"], 
           d18O_T3 = result$d18O[result$Treatment.Number == "3"], 
           SD_T3 = result$StDev[result$Treatment.Number == "3"], 
           d18O_T4 = result$d18O[result$Treatment.Number == "4"], 
           SD_T4 = result$StDev[result$Treatment.Number == "4"], 
           d18O_T5 = result$d18O[result$Treatment.Number == "5"], 
           SD_T5 = result$StDev[result$Treatment.Number == "5"], 
           d18O_T6 = result$d18O[result$Treatment.Number == "6"], 
           SD_T6 = result$StDev[result$Treatment.Number == "6"], 
           d18O_T7 = result$d18O[result$Treatment.Number == "7"], 
           SD_T7 = result$StDev[result$Treatment.Number == "7"])

#Data frame with all rinse water data
df_w = 
  data.frame(d18O_w = treat$Rinse.d18O)

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
